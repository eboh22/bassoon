;; Enhanced Multi-Type Storage Contract
;; Supports multiple data types with advanced features

;; Define the contract owner
(define-data-var contract-owner principal tx-sender)

;; Define data variables for different types
(define-data-var stored-int (optional int) none)
(define-data-var stored-uint (optional uint) none)
(define-data-var stored-string (optional (string-utf8 256)) none)
(define-data-var stored-bool (optional bool) none)
(define-data-var stored-principal (optional principal) none)
(define-data-var stored-buffer (optional (buff 256)) none)

;; Track which type is currently stored
;; 0 = none, 1 = int, 2 = uint, 3 = string, 4 = bool, 5 = principal, 6 = buffer
(define-data-var current-type uint u0)

;; Track storage metadata
(define-data-var last-updated-block uint u0)
(define-data-var update-count uint u0)
(define-data-var last-updater (optional principal) none)

;; Access control settings
(define-data-var is-locked bool false)
(define-data-var allowed-writers (list 10 principal) (list))

;; Define error constants
(define-constant ERR-NO-VALUE u100)
(define-constant ERR-WRONG-TYPE u101)
(define-constant ERR-STRING-TOO-LONG u102)
(define-constant ERR-NOT-AUTHORIZED u103)
(define-constant ERR-CONTRACT-LOCKED u104)
(define-constant ERR-INVALID-TYPE u105)
(define-constant ERR-LIST-FULL u106)

;; Define type constants for clarity
(define-constant TYPE-NONE u0)
(define-constant TYPE-INT u1)
(define-constant TYPE-UINT u2)
(define-constant TYPE-STRING u3)
(define-constant TYPE-BOOL u4)
(define-constant TYPE-PRINCIPAL u5)
(define-constant TYPE-BUFFER u6)

;; Helper function to check if caller is authorized
(define-private (is-authorized)
  (or 
    (is-eq tx-sender (var-get contract-owner))
    (is-some (index-of (var-get allowed-writers) tx-sender))
  )
)

;; Update metadata on each storage operation
(define-private (update-metadata)
  (begin
    (var-set last-updated-block block-height)
    (var-set update-count (+ (var-get update-count) u1))
    (var-set last-updater (some tx-sender))
    (add-to-history)
    true
  )
)

;; Clear all storage variables
(define-private (clear-all-storage)
  (begin
    (var-set stored-int none)
    (var-set stored-uint none)
    (var-set stored-string none)
    (var-set stored-bool none)
    (var-set stored-principal none)
    (var-set stored-buffer none)
    (var-set current-type TYPE-NONE)
  )
)

;; Owner-only function to transfer ownership
(define-public (transfer-ownership (new-owner principal))
  (if (is-eq tx-sender (var-get contract-owner))
    (begin
      (var-set contract-owner new-owner)
      (ok true)
    )
    (err ERR-NOT-AUTHORIZED)
  )
)

;; Owner-only function to lock/unlock the contract
(define-public (set-lock-status (locked bool))
  (if (is-eq tx-sender (var-get contract-owner))
    (begin
      (var-set is-locked locked)
      (ok true)
    )
    (err ERR-NOT-AUTHORIZED)
  )
)

;; Owner-only function to add an allowed writer
(define-public (add-writer (writer principal))
  (if (is-eq tx-sender (var-get contract-owner))
    (let ((current-writers (var-get allowed-writers)))
      (if (< (len current-writers) u10)
        (match (as-max-len? (append current-writers writer) u10)
          new-list (begin
            (var-set allowed-writers new-list)
            (ok true)
          )
          (err ERR-LIST-FULL)
        )
        (err ERR-LIST-FULL)
      )
    )
    (err ERR-NOT-AUTHORIZED)
  )
)

;; Owner-only function to remove a writer
(define-public (remove-writer (writer principal))
  (if (is-eq tx-sender (var-get contract-owner))
    (begin
      (var-set allowed-writers 
        (filter remove-writer-filter (var-get allowed-writers))
      )
      (ok true)
    )
    (err ERR-NOT-AUTHORIZED)
  )
)

(define-private (remove-writer-filter (p principal))
  (not (is-eq p tx-sender))
)

;; Read-only functions for metadata
(define-read-only (get-metadata)
  {
    owner: (var-get contract-owner),
    current-type: (var-get current-type),
    last-updated-block: (var-get last-updated-block),
    update-count: (var-get update-count),
    last-updater: (var-get last-updater),
    is-locked: (var-get is-locked)
  }
)

(define-read-only (get-current-type)
  (var-get current-type)
)

(define-read-only (get-type-name)
  (let ((type (var-get current-type)))
    (if (is-eq type TYPE-INT) "int"
    (if (is-eq type TYPE-UINT) "uint"
    (if (is-eq type TYPE-STRING) "string"
    (if (is-eq type TYPE-BOOL) "bool"
    (if (is-eq type TYPE-PRINCIPAL) "principal"
    (if (is-eq type TYPE-BUFFER) "buffer"
    "none"))))))
  )
)

;; Storage functions for each type
(define-public (store-int (value int))
  (if (var-get is-locked)
    (err ERR-CONTRACT-LOCKED)
    (if (is-authorized)
      (begin
        (clear-all-storage)
        (var-set stored-int (some value))
        (var-set current-type TYPE-INT)
        (update-metadata)
        (print { event: "value-stored", type: "int", sender: tx-sender })
        (ok true)
      )
      (err ERR-NOT-AUTHORIZED)
    )
  )
)

(define-public (store-uint (value uint))
  (if (var-get is-locked)
    (err ERR-CONTRACT-LOCKED)
    (if (is-authorized)
      (begin
        (clear-all-storage)
        (var-set stored-uint (some value))
        (var-set current-type TYPE-UINT)
        (update-metadata)
        (print { event: "value-stored", type: "uint", sender: tx-sender })
        (ok true)
      )
      (err ERR-NOT-AUTHORIZED)
    )
  )
)

(define-public (store-string (value (string-utf8 256)))
  (if (var-get is-locked)
    (err ERR-CONTRACT-LOCKED)
    (if (is-authorized)
      (begin
        (clear-all-storage)
        (var-set stored-string (some value))
        (var-set current-type TYPE-STRING)
        (update-metadata)
        (print { event: "value-stored", type: "string", sender: tx-sender })
        (ok true)
      )
      (err ERR-NOT-AUTHORIZED)
    )
  )
)

(define-public (store-bool (value bool))
  (if (var-get is-locked)
    (err ERR-CONTRACT-LOCKED)
    (if (is-authorized)
      (begin
        (clear-all-storage)
        (var-set stored-bool (some value))
        (var-set current-type TYPE-BOOL)
        (update-metadata)
        (print { event: "value-stored", type: "bool", sender: tx-sender })
        (ok true)
      )
      (err ERR-NOT-AUTHORIZED)
    )
  )
)

(define-public (store-principal (value principal))
  (if (var-get is-locked)
    (err ERR-CONTRACT-LOCKED)
    (if (is-authorized)
      (begin
        (clear-all-storage)
        (var-set stored-principal (some value))
        (var-set current-type TYPE-PRINCIPAL)
        (update-metadata)
        (print { event: "value-stored", type: "principal", sender: tx-sender })
        (ok true)
      )
      (err ERR-NOT-AUTHORIZED)
    )
  )
)

(define-public (store-buffer (value (buff 256)))
  (if (var-get is-locked)
    (err ERR-CONTRACT-LOCKED)
    (if (is-authorized)
      (begin
        (clear-all-storage)
        (var-set stored-buffer (some value))
        (var-set current-type TYPE-BUFFER)
        (update-metadata)
        (print { event: "value-stored", type: "buffer", sender: tx-sender })
        (ok true)
      )
      (err ERR-NOT-AUTHORIZED)
    )
  )
)

;; Getter functions for each type
(define-read-only (get-int)
  (match (var-get stored-int)
    value (ok value)
    (err ERR-NO-VALUE)
  )
)

(define-read-only (get-uint)
  (match (var-get stored-uint)
    value (ok value)
    (err ERR-NO-VALUE)
  )
)

(define-read-only (get-string)
  (match (var-get stored-string)
    value (ok value)
    (err ERR-NO-VALUE)
  )
)

(define-read-only (get-bool)
  (match (var-get stored-bool)
    value (ok value)
    (err ERR-NO-VALUE)
  )
)

(define-read-only (get-principal)
  (match (var-get stored-principal)
    value (ok value)
    (err ERR-NO-VALUE)
  )
)

(define-read-only (get-buffer)
  (match (var-get stored-buffer)
    value (ok value)
    (err ERR-NO-VALUE)
  )
)

;; Universal getter that returns type and value
(define-read-only (get-value)
  (let ((type (var-get current-type)))
    (if (is-eq type TYPE-INT)
      (match (var-get stored-int)
        value (ok { type: "int", has-value: true })
        (err ERR-NO-VALUE))
    (if (is-eq type TYPE-UINT)
      (match (var-get stored-uint)
        value (ok { type: "uint", has-value: true })
        (err ERR-NO-VALUE))
    (if (is-eq type TYPE-STRING)
      (match (var-get stored-string)
        value (ok { type: "string", has-value: true })
        (err ERR-NO-VALUE))
    (if (is-eq type TYPE-BOOL)
      (match (var-get stored-bool)
        value (ok { type: "bool", has-value: true })
        (err ERR-NO-VALUE))
    (if (is-eq type TYPE-PRINCIPAL)
      (match (var-get stored-principal)
        value (ok { type: "principal", has-value: true })
        (err ERR-NO-VALUE))
    (if (is-eq type TYPE-BUFFER)
      (match (var-get stored-buffer)
        value (ok { type: "buffer", has-value: true })
        (err ERR-NO-VALUE))
    (err ERR-NO-VALUE)))))))
  )
)

;; Type checking functions
(define-read-only (is-int) (is-eq (var-get current-type) TYPE-INT))
(define-read-only (is-uint) (is-eq (var-get current-type) TYPE-UINT))
(define-read-only (is-string) (is-eq (var-get current-type) TYPE-STRING))
(define-read-only (is-bool) (is-eq (var-get current-type) TYPE-BOOL))
(define-read-only (is-principal) (is-eq (var-get current-type) TYPE-PRINCIPAL))
(define-read-only (is-buffer) (is-eq (var-get current-type) TYPE-BUFFER))
(define-read-only (has-value) (not (is-eq (var-get current-type) TYPE-NONE)))

;; Clear function with authorization
(define-public (clear-value)
  (if (var-get is-locked)
    (err ERR-CONTRACT-LOCKED)
    (if (is-authorized)
      (begin
        (clear-all-storage)
        (update-metadata)
        (print { event: "value-cleared", sender: tx-sender })
        (ok true)
      )
      (err ERR-NOT-AUTHORIZED)
    )
  )
)

;; Batch operations
(define-public (store-multiple (values (list 10 { type: (string-ascii 10), value: (string-utf8 256) })))
  (if (var-get is-locked)
    (err ERR-CONTRACT-LOCKED)
    (if (is-authorized)
      (begin
        ;; Store the last value from the list
        (map store-from-tuple values)
        (ok true)
      )
      (err ERR-NOT-AUTHORIZED)
    )
  )
)

(define-private (store-from-tuple (entry { type: (string-ascii 10), value: (string-utf8 256) }))
  (let ((type (get type entry))
        (value (get value entry)))
    (if (is-eq type "string")
      (begin
        (clear-all-storage)
        (var-set stored-string (some value))
        (var-set current-type TYPE-STRING)
        (update-metadata)
        true
      )
      false
    )
  )
)

;; History tracking (simplified - stores last 5 operations)
(define-data-var operation-history (list 5 { block: uint, type: uint, updater: principal }) (list))

(define-private (add-to-history)
  (let ((new-entry { block: block-height, type: (var-get current-type), updater: tx-sender })
        (current-history (var-get operation-history)))
    (if (<= (len current-history) u4)
      ;; List has space, just append
      (match (as-max-len? (append current-history new-entry) u5)
        new-list (var-set operation-history new-list)
        false
      )
      ;; List is full, remove oldest and append new
      (match (slice? current-history u1 u5)
        sliced-history 
          (match (as-max-len? (append sliced-history new-entry) u5)
            new-list (var-set operation-history new-list)
            false
          )
        false
      )
    )
  )
)

(define-read-only (get-history)
  (var-get operation-history)
)