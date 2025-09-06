
;; title: asset-management
;; version: 1.0.0
;; summary: Public Works Asset Management System
;; description: A comprehensive infrastructure maintenance system with asset inventory, condition assessment, repair scheduling, and budget planning for municipal services.

;; constants
(define-constant CONTRACT-OWNER tx-sender)
(define-constant ERR-NOT-AUTHORIZED (err u100))
(define-constant ERR-ASSET-NOT-FOUND (err u101))
(define-constant ERR-INVALID-CONDITION (err u102))
(define-constant ERR-REPAIR-NOT-FOUND (err u103))
(define-constant ERR-INSUFFICIENT-BUDGET (err u104))
(define-constant ERR-INVALID-PRIORITY (err u105))
(define-constant ERR-ASSET-ALREADY-EXISTS (err u106))

;; Asset condition ratings: 1=Poor, 2=Fair, 3=Good, 4=Excellent
(define-constant CONDITION-POOR u1)
(define-constant CONDITION-FAIR u2)
(define-constant CONDITION-GOOD u3)
(define-constant CONDITION-EXCELLENT u4)

;; Repair priorities: 1=Critical, 2=High, 3=Medium, 4=Low
(define-constant PRIORITY-CRITICAL u1)
(define-constant PRIORITY-HIGH u2)
(define-constant PRIORITY-MEDIUM u3)
(define-constant PRIORITY-LOW u4)

;; data vars
(define-data-var next-asset-id uint u1)
(define-data-var next-repair-id uint u1)
(define-data-var total-budget uint u0)
(define-data-var allocated-budget uint u0)

;; data maps
;; Asset inventory with comprehensive details
(define-map assets
  { asset-id: uint }
  {
    asset-type: (string-ascii 50),
    location: (string-ascii 100),
    installation-date: uint,
    condition-rating: uint,
    last-inspection: uint,
    estimated-value: uint,
    maintenance-cost: uint,
    is-active: bool
  }
)

;; Asset condition assessments
(define-map asset-assessments
  { asset-id: uint, assessment-date: uint }
  {
    inspector: principal,
    condition-rating: uint,
    notes: (string-ascii 200),
    recommended-repairs: (string-ascii 300)
  }
)

;; Repair scheduling and tracking
(define-map repair-schedules
  { repair-id: uint }
  {
    asset-id: uint,
    priority: uint,
    description: (string-ascii 200),
    estimated-cost: uint,
    scheduled-date: uint,
    assigned-contractor: (optional principal),
    status: (string-ascii 20),
    completion-date: (optional uint)
  }
)

;; Budget allocations by category
(define-map budget-allocations
  { category: (string-ascii 50) }
  { allocated-amount: uint, spent-amount: uint }
)

;; Asset maintenance history
(define-map maintenance-history
  { asset-id: uint, maintenance-date: uint }
  {
    repair-type: (string-ascii 100),
    cost: uint,
    contractor: principal,
    description: (string-ascii 200)
  }
)

;; public functions

;; Add new asset to inventory
(define-public (add-asset
  (asset-type (string-ascii 50))
  (location (string-ascii 100))
  (installation-date uint)
  (condition-rating uint)
  (estimated-value uint)
  (maintenance-cost uint))
  (let ((asset-id (var-get next-asset-id)))
    (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-NOT-AUTHORIZED)
    (asserts! (and (>= condition-rating CONDITION-POOR) (<= condition-rating CONDITION-EXCELLENT)) ERR-INVALID-CONDITION)
    (asserts! (is-none (map-get? assets { asset-id: asset-id })) ERR-ASSET-ALREADY-EXISTS)
    
    (map-set assets
      { asset-id: asset-id }
      {
        asset-type: asset-type,
        location: location,
        installation-date: installation-date,
        condition-rating: condition-rating,
        last-inspection: stacks-block-height,
        estimated-value: estimated-value,
        maintenance-cost: maintenance-cost,
        is-active: true
      }
    )
    
    (var-set next-asset-id (+ asset-id u1))
    (ok asset-id)
  )
)

;; Update asset condition after inspection
(define-public (update-asset-condition
  (asset-id uint)
  (new-condition uint)
  (notes (string-ascii 200))
  (recommended-repairs (string-ascii 300)))
  (let ((asset (unwrap! (map-get? assets { asset-id: asset-id }) ERR-ASSET-NOT-FOUND)))
    (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-NOT-AUTHORIZED)
    (asserts! (and (>= new-condition CONDITION-POOR) (<= new-condition CONDITION-EXCELLENT)) ERR-INVALID-CONDITION)
    
    ;; Update asset condition
    (map-set assets
      { asset-id: asset-id }
      (merge asset { condition-rating: new-condition, last-inspection: stacks-block-height })
    )
    
    ;; Record assessment
    (map-set asset-assessments
      { asset-id: asset-id, assessment-date: stacks-block-height }
      {
        inspector: tx-sender,
        condition-rating: new-condition,
        notes: notes,
        recommended-repairs: recommended-repairs
      }
    )
    
    (ok true)
  )
)

;; Schedule repair work
(define-public (schedule-repair
  (asset-id uint)
  (priority uint)
  (description (string-ascii 200))
  (estimated-cost uint)
  (scheduled-date uint))
  (let ((repair-id (var-get next-repair-id))
        (available-budget (- (var-get total-budget) (var-get allocated-budget))))
    (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-NOT-AUTHORIZED)
    (asserts! (is-some (map-get? assets { asset-id: asset-id })) ERR-ASSET-NOT-FOUND)
    (asserts! (and (>= priority PRIORITY-CRITICAL) (<= priority PRIORITY-LOW)) ERR-INVALID-PRIORITY)
    (asserts! (>= available-budget estimated-cost) ERR-INSUFFICIENT-BUDGET)
    
    (map-set repair-schedules
      { repair-id: repair-id }
      {
        asset-id: asset-id,
        priority: priority,
        description: description,
        estimated-cost: estimated-cost,
        scheduled-date: scheduled-date,
        assigned-contractor: none,
        status: "scheduled",
        completion-date: none
      }
    )
    
    ;; Allocate budget for this repair
    (var-set allocated-budget (+ (var-get allocated-budget) estimated-cost))
    (var-set next-repair-id (+ repair-id u1))
    (ok repair-id)
  )
)

;; Assign contractor to repair
(define-public (assign-contractor (repair-id uint) (contractor principal))
  (let ((repair (unwrap! (map-get? repair-schedules { repair-id: repair-id }) ERR-REPAIR-NOT-FOUND)))
    (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-NOT-AUTHORIZED)
    
    (map-set repair-schedules
      { repair-id: repair-id }
      (merge repair { assigned-contractor: (some contractor), status: "assigned" })
    )
    
    (ok true)
  )
)

;; Complete repair work
(define-public (complete-repair
  (repair-id uint)
  (actual-cost uint)
  (repair-type (string-ascii 100))
  (description (string-ascii 200)))
  (let ((repair (unwrap! (map-get? repair-schedules { repair-id: repair-id }) ERR-REPAIR-NOT-FOUND))
        (contractor (unwrap! (get assigned-contractor repair) ERR-NOT-AUTHORIZED)))
    (asserts! (is-eq tx-sender contractor) ERR-NOT-AUTHORIZED)
    
    ;; Update repair status
    (map-set repair-schedules
      { repair-id: repair-id }
      (merge repair { status: "completed", completion-date: (some stacks-block-height) })
    )
    
    ;; Record maintenance history
    (map-set maintenance-history
      { asset-id: (get asset-id repair), maintenance-date: stacks-block-height }
      {
        repair-type: repair-type,
        cost: actual-cost,
        contractor: contractor,
        description: description
      }
    )
    
    ;; Adjust allocated budget
    (var-set allocated-budget (- (var-get allocated-budget) (get estimated-cost repair)))
    (ok true)
  )
)

;; Set total budget
(define-public (set-total-budget (amount uint))
  (begin
    (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-NOT-AUTHORIZED)
    (var-set total-budget amount)
    (ok true)
  )
)

;; Allocate budget by category
(define-public (allocate-budget (category (string-ascii 50)) (amount uint))
  (let ((available-budget (- (var-get total-budget) (var-get allocated-budget))))
    (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-NOT-AUTHORIZED)
    (asserts! (>= available-budget amount) ERR-INSUFFICIENT-BUDGET)
    
    (map-set budget-allocations
      { category: category }
      { allocated-amount: amount, spent-amount: u0 }
    )
    
    (ok true)
  )
)

;; read only functions

;; Get asset details
(define-read-only (get-asset (asset-id uint))
  (map-get? assets { asset-id: asset-id })
)

;; Get asset assessment
(define-read-only (get-asset-assessment (asset-id uint) (assessment-date uint))
  (map-get? asset-assessments { asset-id: asset-id, assessment-date: assessment-date })
)

;; Get repair schedule
(define-read-only (get-repair-schedule (repair-id uint))
  (map-get? repair-schedules { repair-id: repair-id })
)

;; Get maintenance history
(define-read-only (get-maintenance-history (asset-id uint) (maintenance-date uint))
  (map-get? maintenance-history { asset-id: asset-id, maintenance-date: maintenance-date })
)

;; Get budget allocation
(define-read-only (get-budget-allocation (category (string-ascii 50)))
  (map-get? budget-allocations { category: category })
)

;; Get budget summary
(define-read-only (get-budget-summary)
  {
    total-budget: (var-get total-budget),
    allocated-budget: (var-get allocated-budget),
    available-budget: (- (var-get total-budget) (var-get allocated-budget))
  }
)

;; Get next asset ID
(define-read-only (get-next-asset-id)
  (var-get next-asset-id)
)

;; Get next repair ID
(define-read-only (get-next-repair-id)
  (var-get next-repair-id)
)

;; Get assets by condition (helper function for filtering)
(define-read-only (get-asset-condition (asset-id uint))
  (match (map-get? assets { asset-id: asset-id })
    asset (some (get condition-rating asset))
    none
  )
)

;; Check if asset needs repair (condition <= 2)
(define-read-only (asset-needs-repair (asset-id uint))
  (match (map-get? assets { asset-id: asset-id })
    asset (<= (get condition-rating asset) CONDITION-FAIR)
    false
  )
)
