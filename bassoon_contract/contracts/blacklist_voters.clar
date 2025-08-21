;; Advanced Voting with Blacklist Contract
;; Enhanced features: Multiple proposals, delegation, weighted voting, time limits, and more

;; Constants
(define-constant contract-owner tx-sender)
(define-constant err-owner-only (err u100))
(define-constant err-blacklisted (err u101))
(define-constant err-already-voted (err u102))
(define-constant err-invalid-option (err u103))
(define-constant err-not-found (err u104))
(define-constant err-voting-inactive (err u105))
(define-constant err-voting-ended (err u106))
(define-constant err-invalid-proposal (err u107))
(define-constant err-insufficient-weight (err u108))
(define-constant err-delegation-cycle (err u109))
(define-constant err-invalid-quorum (err u110))
(define-constant err-unauthorized (err u111))

;; Data Variables
(define-data-var voting-active bool false)
(define-data-var current-proposal-id uint u0)
(define-data-var voting-end-block uint u0)
(define-data-var min-quorum uint u10) ;; Minimum votes required
(define-data-var admin-count uint u1)

;; Data Maps
(define-map blacklisted-addresses principal bool)
(define-map voter-weights principal uint) ;; Weighted voting
(define-map vote-delegations principal principal) ;; Vote delegation
(define-map admin-addresses principal bool) ;; Multiple admins

;; Proposal structure
(define-map proposals uint {
  title: (string-utf8 100),
  description: (string-utf8 500),
  creator: principal,
  created-at: uint,
  end-block: uint,
  active: bool,
  yes-votes: uint,
  no-votes: uint,
  abstain-votes: uint,
  total-weight: uint,
  quorum-met: bool,
  executed: bool
})

;; Vote records for each proposal
(define-map proposal-votes {proposal-id: uint, voter: principal} {
  vote-option: uint, ;; 0=no, 1=yes, 2=abstain
  weight: uint,
  timestamp: uint
})

;; Voter participation tracking
(define-map voter-stats principal {
  total-votes: uint,
  last-vote-block: uint,
  reputation-score: uint
})

;; Proposal categories and permissions
(define-map proposal-categories (string-ascii 20) {
  min-weight: uint,
  admin-only: bool
})

;; Read-only functions

;; Check if an address is blacklisted
(define-read-only (is-blacklisted (address principal))
  (default-to false (map-get? blacklisted-addresses address)))

;; Check if address is admin
(define-read-only (is-admin (address principal))
  (or (is-eq address contract-owner) 
      (default-to false (map-get? admin-addresses address))))

;; Get voter weight (default 1 if not set)
(define-read-only (get-voter-weight (address principal))
  (default-to u1 (map-get? voter-weights address)))

;; Get effective voting weight (base weight only for now)
(define-read-only (get-effective-weight (voter principal))
  ;; If voter has delegated their vote, they can't vote directly
  (if (has-delegated-vote voter)
    u0
    (get-voter-weight voter)))

;; Calculate delegated weight received (simplified approach)
(define-read-only (get-delegated-weight (delegate principal))
  ;; In a full implementation, this would require maintaining a reverse mapping
  ;; For now, we return 0 as delegation weight calculation requires off-chain indexing
  u0)

;; Check who a voter has delegated to
(define-read-only (get-delegation-target (delegator principal))
  (map-get? vote-delegations delegator))

;; Helper function to check if address has delegated their vote
(define-read-only (has-delegated-vote (voter principal))
  (is-some (map-get? vote-delegations voter)))

;; Check if voter has voted on specific proposal
(define-read-only (has-voted-on-proposal (proposal-id uint) (voter principal))
  (is-some (map-get? proposal-votes {proposal-id: proposal-id, voter: voter})))

;; Get proposal details
(define-read-only (get-proposal (proposal-id uint))
  (map-get? proposals proposal-id))

;; Get vote details for a voter on a proposal
(define-read-only (get-vote-details (proposal-id uint) (voter principal))
  (map-get? proposal-votes {proposal-id: proposal-id, voter: voter}))

;; Get current voting statistics
(define-read-only (get-voting-stats)
  {
    active: (var-get voting-active),
    current-proposal: (var-get current-proposal-id),
    end-block: (var-get voting-end-block),
    min-quorum: (var-get min-quorum),
    block-height: block-height
  })

;; Get voter statistics
(define-read-only (get-voter-stats (voter principal))
  (default-to {total-votes: u0, last-vote-block: u0, reputation-score: u0}
    (map-get? voter-stats voter)))

;; Check if proposal has met quorum
(define-read-only (has-met-quorum (proposal-id uint))
  (match (map-get? proposals proposal-id)
    proposal (>= (get total-weight proposal) (var-get min-quorum))
    false))

;; Check if voting period has ended
(define-read-only (is-voting-ended (proposal-id uint))
  (match (map-get? proposals proposal-id)
    proposal (>= block-height (get end-block proposal))
    true))

;; Admin functions

;; Add new admin
(define-public (add-admin (new-admin principal))
  (begin
    (asserts! (is-admin tx-sender) err-owner-only)
    (map-set admin-addresses new-admin true)
    (var-set admin-count (+ (var-get admin-count) u1))
    (ok true)))

;; Remove admin
(define-public (remove-admin (admin principal))
  (begin
    (asserts! (is-admin tx-sender) err-owner-only)
    (asserts! (not (is-eq admin contract-owner)) err-unauthorized)
    (map-delete admin-addresses admin)
    (var-set admin-count (- (var-get admin-count) u1))
    (ok true)))

;; Blacklist management
(define-public (blacklist-address (address principal))
  (begin
    (asserts! (is-admin tx-sender) err-owner-only)
    (map-set blacklisted-addresses address true)
    (ok true)))

(define-public (unblacklist-address (address principal))
  (begin
    (asserts! (is-admin tx-sender) err-owner-only)
    (map-delete blacklisted-addresses address)
    (ok true)))

;; Set voter weight
(define-public (set-voter-weight (voter principal) (weight uint))
  (begin
    (asserts! (is-admin tx-sender) err-owner-only)
    (asserts! (> weight u0) err-insufficient-weight)
    (map-set voter-weights voter weight)
    (ok true)))

;; Batch set voter weights
(define-public (batch-set-weights (voters-weights (list 25 {voter: principal, weight: uint})))
  (begin
    (asserts! (is-admin tx-sender) err-owner-only)
    (fold set-single-weight voters-weights (ok true))))

(define-private (set-single-weight (voter-weight {voter: principal, weight: uint}) (prev-result (response bool uint)))
  (match prev-result
    success (begin
              (map-set voter-weights (get voter voter-weight) (get weight voter-weight))
              (ok true))
    error prev-result))

;; Create new proposal
(define-public (create-proposal 
  (title (string-utf8 100)) 
  (description (string-utf8 500)) 
  (duration-blocks uint))
  (let ((proposal-id (+ (var-get current-proposal-id) u1))
        (end-block (+ block-height duration-blocks)))
    (begin
      (asserts! (is-admin tx-sender) err-owner-only)
      (asserts! (> duration-blocks u0) err-invalid-proposal)
      
      (map-set proposals proposal-id {
        title: title,
        description: description,
        creator: tx-sender,
        created-at: block-height,
        end-block: end-block,
        active: true,
        yes-votes: u0,
        no-votes: u0,
        abstain-votes: u0,
        total-weight: u0,
        quorum-met: false,
        executed: false
      })
      
      (var-set current-proposal-id proposal-id)
      (var-set voting-active true)
      (var-set voting-end-block end-block)
      (ok proposal-id))))

;; End proposal voting
(define-public (end-proposal (proposal-id uint))
  (begin
    (asserts! (is-admin tx-sender) err-owner-only)
    (match (map-get? proposals proposal-id)
      proposal (begin
        (map-set proposals proposal-id (merge proposal {active: false}))
        (if (is-eq proposal-id (var-get current-proposal-id))
          (var-set voting-active false)
          true)
        (ok true))
      err-not-found)))

;; Set minimum quorum
(define-public (set-quorum (new-quorum uint))
  (begin
    (asserts! (is-admin tx-sender) err-owner-only)
    (asserts! (> new-quorum u0) err-invalid-quorum)
    (var-set min-quorum new-quorum)
    (ok true)))

;; Public functions

;; Delegate vote to another address
(define-public (delegate-vote (delegate-to principal))
  (begin
    (asserts! (not (is-blacklisted tx-sender)) err-blacklisted)
    (asserts! (not (is-blacklisted delegate-to)) err-blacklisted)
    (asserts! (not (is-eq tx-sender delegate-to)) err-delegation-cycle)
    
    ;; Check for delegation cycles (simplified check)
    (asserts! (not (is-eq (unwrap! (map-get? vote-delegations delegate-to) (ok true)) tx-sender)) 
              err-delegation-cycle)
    
    (map-set vote-delegations tx-sender delegate-to)
    (ok true)))

;; Remove vote delegation
(define-public (remove-delegation)
  (begin
    (map-delete vote-delegations tx-sender)
    (ok true)))

;; Cast vote on proposal (0=no, 1=yes, 2=abstain)
(define-public (cast-vote (proposal-id uint) (vote-option uint))
  (let ((voter tx-sender)
        (voter-weight (get-effective-weight voter)))
    (begin
      ;; Validation checks
      (asserts! (not (is-blacklisted voter)) err-blacklisted)
      (asserts! (<= vote-option u2) err-invalid-option)
      (asserts! (not (has-voted-on-proposal proposal-id voter)) err-already-voted)
      
      ;; Check proposal exists and is active
      (match (map-get? proposals proposal-id)
        proposal (begin
          (asserts! (get active proposal) err-voting-inactive)
          (asserts! (< block-height (get end-block proposal)) err-voting-ended)
          
          ;; Record vote
          (map-set proposal-votes {proposal-id: proposal-id, voter: voter} {
            vote-option: vote-option,
            weight: voter-weight,
            timestamp: block-height
          })
          
          ;; Update proposal vote counts
          (let ((updated-proposal 
            (merge proposal {
              yes-votes: (if (is-eq vote-option u1) 
                          (+ (get yes-votes proposal) voter-weight) 
                          (get yes-votes proposal)),
              no-votes: (if (is-eq vote-option u0) 
                         (+ (get no-votes proposal) voter-weight) 
                         (get no-votes proposal)),
              abstain-votes: (if (is-eq vote-option u2) 
                              (+ (get abstain-votes proposal) voter-weight) 
                              (get abstain-votes proposal)),
              total-weight: (+ (get total-weight proposal) voter-weight),
              quorum-met: (>= (+ (get total-weight proposal) voter-weight) (var-get min-quorum))
            })))
            
            (map-set proposals proposal-id updated-proposal)
            
            ;; Update voter stats
            (let ((current-stats (get-voter-stats voter)))
              (map-set voter-stats voter {
                total-votes: (+ (get total-votes current-stats) u1),
                last-vote-block: block-height,
                reputation-score: (+ (get reputation-score current-stats) u1)
              }))
            
            (ok true)))
        err-not-found))))

;; Emergency functions

;; Emergency pause (only owner)
(define-public (emergency-pause)
  (begin
    (asserts! (is-eq tx-sender contract-owner) err-owner-only)
    (var-set voting-active false)
    (ok true)))

;; Batch operations for efficiency

;; Get multiple proposal details
(define-read-only (get-proposals-batch (proposal-ids (list 10 uint)))
  (map get-proposal proposal-ids))

;; Get voting results for proposal
(define-read-only (get-proposal-results (proposal-id uint))
  (match (map-get? proposals proposal-id)
    proposal (some {
      proposal-id: proposal-id,
      title: (get title proposal),
      yes-votes: (get yes-votes proposal),
      no-votes: (get no-votes proposal),
      abstain-votes: (get abstain-votes proposal),
      total-weight: (get total-weight proposal),
      quorum-met: (get quorum-met proposal),
      active: (get active proposal),
      ended: (>= block-height (get end-block proposal))
    })
    none))

;; Get leaderboard of top voters by reputation
(define-read-only (get-voter-leaderboard)
  ;; This would require additional indexing in a full implementation
  (ok "Leaderboard feature requires off-chain indexing"))