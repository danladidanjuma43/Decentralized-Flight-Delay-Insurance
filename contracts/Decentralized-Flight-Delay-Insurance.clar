(define-constant contract-owner tx-sender)
(define-constant err-owner-only (err u100))
(define-constant err-invalid-flight (err u101))
(define-constant err-policy-exists (err u102))
(define-constant err-no-policy (err u103))
(define-constant err-insufficient-funds (err u104))
(define-constant err-not-claimable (err u105))
(define-constant err-already-claimed (err u106))
(define-constant minimum-delay u120)
(define-constant policy-price u100000000)
(define-constant payout-amount u300000000)

(define-data-var oracle-address principal 'ST1PQHQKV0RJXZFY1DGX8MNSNYVE3VGZJSRTPGZGM)

(define-map flight-policies
    {
        flight-number: (string-ascii 10),
        departure-time: uint,
    }
    {
        owner: principal,
        delay-minutes: uint,
        claimed: bool,
        active: bool,
    }
)

(define-map flight-data
    { flight-number: (string-ascii 10) }
    {
        actual-departure: uint,
        status: (string-ascii 20),
    }
)

(define-public (set-oracle-address (new-oracle principal))
    (begin
        (asserts! (is-eq tx-sender contract-owner) err-owner-only)
        (ok (var-set oracle-address new-oracle))
    )
)

(define-read-only (get-policy
        (flight-number (string-ascii 10))
        (departure-time uint)
    )
    (map-get? flight-policies {
        flight-number: flight-number,
        departure-time: departure-time,
    })
)

(define-public (purchase-policy
        (flight-number (string-ascii 10))
        (departure-time uint)
    )
    (let ((existing-policy (get-policy flight-number departure-time)))
        (asserts! (is-none existing-policy) err-policy-exists)
        (try! (stx-transfer? policy-price tx-sender contract-owner))
        (ok (map-set flight-policies {
            flight-number: flight-number,
            departure-time: departure-time,
        } {
            owner: tx-sender,
            delay-minutes: u0,
            claimed: false,
            active: true,
        }))
    )
)

(define-public (update-flight-data
        (flight-number (string-ascii 10))
        (actual-departure uint)
        (status (string-ascii 20))
    )
    (begin
        (asserts! (is-eq tx-sender (var-get oracle-address)) err-owner-only)
        (ok (map-set flight-data { flight-number: flight-number } {
            actual-departure: actual-departure,
            status: status,
        }))
    )
)

(define-read-only (get-flight-data (flight-number (string-ascii 10)))
    (map-get? flight-data { flight-number: flight-number })
)

(define-public (record-delay
        (flight-number (string-ascii 10))
        (departure-time uint)
        (delay-minutes uint)
    )
    (begin
        (asserts! (is-eq tx-sender (var-get oracle-address)) err-owner-only)
        (ok (map-set flight-policies {
            flight-number: flight-number,
            departure-time: departure-time,
        } {
            owner: (get owner
                (unwrap! (get-policy flight-number departure-time) err-no-policy)
            ),
            delay-minutes: delay-minutes,
            claimed: false,
            active: true,
        }))
    )
)

(define-public (claim-insurance
        (flight-number (string-ascii 10))
        (departure-time uint)
    )
    (let (
            (policy (unwrap! (get-policy flight-number departure-time) err-no-policy))
            (delay (get delay-minutes policy))
        )
        (asserts! (is-eq (get owner policy) tx-sender) err-owner-only)
        (asserts! (not (get claimed policy)) err-already-claimed)
        (asserts! (>= delay minimum-delay) err-not-claimable)
        (try! (stx-transfer? payout-amount contract-owner tx-sender))
        (ok (map-set flight-policies {
            flight-number: flight-number,
            departure-time: departure-time,
        } {
            owner: tx-sender,
            delay-minutes: delay,
            claimed: true,
            active: false,
        }))
    )
)

(define-public (cancel-policy
        (flight-number (string-ascii 10))
        (departure-time uint)
    )
    (let ((policy (unwrap! (get-policy flight-number departure-time) err-no-policy)))
        (asserts! (is-eq (get owner policy) tx-sender) err-owner-only)
        (asserts! (not (get claimed policy)) err-already-claimed)
        (try! (stx-transfer? (/ policy-price u2) contract-owner tx-sender))
        (ok (map-set flight-policies {
            flight-number: flight-number,
            departure-time: departure-time,
        } {
            owner: tx-sender,
            delay-minutes: u0,
            claimed: false,
            active: false,
        }))
    )
)
