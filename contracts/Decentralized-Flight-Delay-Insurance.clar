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
(define-constant err-invalid-tier (err u107))
(define-constant err-invalid-score (err u108))
(define-constant err-claim-not-ready (err u109))
(define-constant err-mismatched-lists (err u110))
(define-constant err-invalid-bundle-size (err u111))
(define-constant err-bundle-too-large (err u112))

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

(define-constant tier-basic-multiplier u100)
(define-constant tier-standard-multiplier u150)
(define-constant tier-premium-multiplier u200)

(define-map route-risk-tiers
    { route: (string-ascii 20) }
    { risk-tier: uint }
)

(define-map airline-reliability
    { airline-code: (string-ascii 5) }
    { reliability-score: uint }
)

(define-public (set-route-risk-tier
        (route (string-ascii 20))
        (risk-tier uint)
    )
    (begin
        (asserts! (is-eq tx-sender contract-owner) err-owner-only)
        (asserts! (<= risk-tier u3) (err u107))
        (ok (map-set route-risk-tiers { route: route } { risk-tier: risk-tier }))
    )
)

(define-public (set-airline-reliability
        (airline-code (string-ascii 5))
        (reliability-score uint)
    )
    (begin
        (asserts! (is-eq tx-sender contract-owner) err-owner-only)
        (asserts! (<= reliability-score u100) (err u108))
        (ok (map-set airline-reliability { airline-code: airline-code } { reliability-score: reliability-score }))
    )
)

(define-read-only (calculate-premium
        (route (string-ascii 20))
        (airline-code (string-ascii 5))
    )
    (let (
            (route-data (map-get? route-risk-tiers { route: route }))
            (airline-data (map-get? airline-reliability { airline-code: airline-code }))
            (base-tier (if (is-some route-data)
                (get risk-tier (unwrap-panic route-data))
                u1
            ))
            (reliability (if (is-some airline-data)
                (get reliability-score (unwrap-panic airline-data))
                u50
            ))
            (tier-multiplier (if (is-eq base-tier u1)
                tier-basic-multiplier
                (if (is-eq base-tier u2)
                    tier-standard-multiplier
                    tier-premium-multiplier
                )
            ))
            (reliability-adjustment (if (>= reliability u80)
                u90
                (if (>= reliability u60)
                    u100
                    u120
                )
            ))
        )
        (/ (* (* policy-price tier-multiplier) reliability-adjustment) u10000)
    )
)

(define-public (purchase-tiered-policy
        (flight-number (string-ascii 10))
        (departure-time uint)
        (route (string-ascii 20))
        (airline-code (string-ascii 5))
    )
    (let (
            (existing-policy (get-policy flight-number departure-time))
            (calculated-premium (calculate-premium route airline-code))
        )
        (asserts! (is-none existing-policy) err-policy-exists)
        (try! (stx-transfer? calculated-premium tx-sender contract-owner))
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
(define-constant auto-claim-window u144)

(define-map pending-auto-claims
    {
        flight-number: (string-ascii 10),
        departure-time: uint,
    }
    {
        eligible-block: uint,
        processed: bool,
    }
)

(define-public (trigger-auto-claim-eligibility
        (flight-number (string-ascii 10))
        (departure-time uint)
    )
    (let (
            (policy (unwrap! (get-policy flight-number departure-time) err-no-policy))
            (delay (get delay-minutes policy))
        )
        (asserts! (>= delay minimum-delay) err-not-claimable)
        (asserts! (not (get claimed policy)) err-already-claimed)
        (ok (map-set pending-auto-claims {
            flight-number: flight-number,
            departure-time: departure-time,
        } {
            eligible-block: (+ burn-block-height auto-claim-window),
            processed: false,
        }))
    )
)

(define-public (execute-auto-claim
        (flight-number (string-ascii 10))
        (departure-time uint)
    )
    (let (
            (policy (unwrap! (get-policy flight-number departure-time) err-no-policy))
            (auto-claim (unwrap!
                (map-get? pending-auto-claims {
                    flight-number: flight-number,
                    departure-time: departure-time,
                })
                err-no-policy
            ))
            (policy-owner (get owner policy))
        )
        (asserts! (>= burn-block-height (get eligible-block auto-claim))
            (err u109)
        )
        (asserts! (not (get processed auto-claim)) err-already-claimed)
        (asserts! (not (get claimed policy)) err-already-claimed)
        (try! (stx-transfer? payout-amount contract-owner policy-owner))
        (map-set pending-auto-claims {
            flight-number: flight-number,
            departure-time: departure-time,
        } {
            eligible-block: (get eligible-block auto-claim),
            processed: true,
        })
        (ok (map-set flight-policies {
            flight-number: flight-number,
            departure-time: departure-time,
        } {
            owner: policy-owner,
            delay-minutes: (get delay-minutes policy),
            claimed: true,
            active: false,
        }))
    )
)

(define-read-only (get-auto-claim-status
        (flight-number (string-ascii 10))
        (departure-time uint)
    )
    (map-get? pending-auto-claims {
        flight-number: flight-number,
        departure-time: departure-time,
    })
)

(define-read-only (is-auto-claim-ready
        (flight-number (string-ascii 10))
        (departure-time uint)
    )
    (match (get-auto-claim-status flight-number departure-time)
        auto-claim (and
            (>= burn-block-height (get eligible-block auto-claim))
            (not (get processed auto-claim))
        )
        false
    )
)

(define-constant bundle-discount-2 u95)
(define-constant bundle-discount-3 u90)
(define-constant bundle-discount-5 u85)
(define-constant max-bundle-size u10)

(define-map bundle-policies
    { bundle-id: uint }
    {
        owner: principal,
        policy-count: uint,
        created-at: uint,
    }
)

(define-data-var bundle-counter uint u0)

(define-map bundle-policy-details
    {
        bundle-id: uint,
        policy-index: uint,
    }
    {
        flight-number: (string-ascii 10),
        departure-time: uint,
        premium-paid: uint,
    }
)

(define-read-only (calculate-bundle-discount (policy-count uint))
    (if (>= policy-count u5)
        bundle-discount-5
        (if (>= policy-count u3)
            bundle-discount-3
            (if (>= policy-count u2)
                bundle-discount-2
                u100
            )
        )
    )
)

(define-public (purchase-bundle-policy
        (flight-numbers (list 10 (string-ascii 10)))
        (departure-times (list 10 uint))
        (routes (list 10 (string-ascii 20)))
        (airline-codes (list 10 (string-ascii 5)))
    )
    (let (
            (policy-count (len flight-numbers))
            (bundle-id (+ (var-get bundle-counter) u1))
        )
        (asserts! (is-eq policy-count (len departure-times)) err-mismatched-lists)
        (asserts! (is-eq policy-count (len routes)) err-mismatched-lists)
        (asserts! (is-eq policy-count (len airline-codes)) err-mismatched-lists)
        (asserts! (> policy-count u1) err-invalid-bundle-size)
        (asserts! (<= policy-count max-bundle-size) err-bundle-too-large)
        (try! (process-bundle-purchase bundle-id flight-numbers departure-times routes
            airline-codes policy-count
        ))
        (var-set bundle-counter bundle-id)
        (ok bundle-id)
    )
)

(define-private (process-bundle-purchase
        (bundle-id uint)
        (flight-numbers (list 10 (string-ascii 10)))
        (departure-times (list 10 uint))
        (routes (list 10 (string-ascii 20)))
        (airline-codes (list 10 (string-ascii 5)))
        (policy-count uint)
    )
    (let (
            (discount-multiplier (calculate-bundle-discount policy-count))
            (total-premium (fold calculate-and-sum-premiums
                (zip flight-numbers departure-times routes airline-codes) u0
            ))
            (discounted-total (/ (* total-premium discount-multiplier) u100))
        )
        (try! (stx-transfer? discounted-total tx-sender contract-owner))
        (fold process-single-bundle-policy
            (zip flight-numbers departure-times routes airline-codes) {
            bundle-id: bundle-id,
            index: u0,
            success: true,
        })
        (ok (map-set bundle-policies { bundle-id: bundle-id } {
            owner: tx-sender,
            policy-count: policy-count,
            created-at: burn-block-height,
        }))
    )
)

(define-private (calculate-and-sum-premiums
        (policy-data {
            flight-number: (string-ascii 10),
            departure-time: uint,
            route: (string-ascii 20),
            airline-code: (string-ascii 5),
        })
        (accumulator uint)
    )
    (+ accumulator
        (calculate-premium (get route policy-data) (get airline-code policy-data))
    )
)

(define-private (process-single-bundle-policy
        (policy-data {
            flight-number: (string-ascii 10),
            departure-time: uint,
            route: (string-ascii 20),
            airline-code: (string-ascii 5),
        })
        (state {
            bundle-id: uint,
            index: uint,
            success: bool,
        })
    )
    (let (
            (flight-number (get flight-number policy-data))
            (departure-time (get departure-time policy-data))
            (route (get route policy-data))
            (airline-code (get airline-code policy-data))
            (bundle-id (get bundle-id state))
            (current-index (get index state))
            (premium (calculate-premium route airline-code))
        )
        (if (get success state)
            (begin
                (map-set flight-policies {
                    flight-number: flight-number,
                    departure-time: departure-time,
                } {
                    owner: tx-sender,
                    delay-minutes: u0,
                    claimed: false,
                    active: true,
                })
                (map-set bundle-policy-details {
                    bundle-id: bundle-id,
                    policy-index: current-index,
                } {
                    flight-number: flight-number,
                    departure-time: departure-time,
                    premium-paid: premium,
                })
                {
                    bundle-id: bundle-id,
                    index: (+ current-index u1),
                    success: true,
                }
            )
            state
        )
    )
)

(define-private (zip
        (list1 (list 10 (string-ascii 10)))
        (list2 (list 10 uint))
        (list3 (list 10 (string-ascii 20)))
        (list4 (list 10 (string-ascii 5)))
    )
    (map combine-elements list1 list2 list3 list4)
)

(define-private (combine-elements
        (flight-number (string-ascii 10))
        (departure-time uint)
        (route (string-ascii 20))
        (airline-code (string-ascii 5))
    )
    {
        flight-number: flight-number,
        departure-time: departure-time,
        route: route,
        airline-code: airline-code,
    }
)

(define-read-only (get-bundle-info (bundle-id uint))
    (map-get? bundle-policies { bundle-id: bundle-id })
)

(define-read-only (get-bundle-policy-details
        (bundle-id uint)
        (policy-index uint)
    )
    (map-get? bundle-policy-details {
        bundle-id: bundle-id,
        policy-index: policy-index,
    })
)
