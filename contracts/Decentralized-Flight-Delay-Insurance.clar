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
(define-constant err-not-transferable (err u113))
(define-constant err-invalid-transfer-price (err u114))
(define-constant err-transfer-not-found (err u115))
(define-constant err-cannot-transfer-to-self (err u116))
(define-constant err-insufficient-stake (err u117))
(define-constant err-no-stake-found (err u118))
(define-constant err-insufficient-pool-balance (err u119))
(define-constant err-cooldown-active (err u120))

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

(define-constant transfer-fee-percentage u5)

(define-map policy-transfers
    {
        flight-number: (string-ascii 10),
        departure-time: uint,
    }
    {
        seller: principal,
        transfer-price: uint,
        listed-at: uint,
        expires-at: uint,
    }
)

(define-public (list-policy-for-transfer
        (flight-number (string-ascii 10))
        (departure-time uint)
        (transfer-price uint)
        (expires-after-blocks uint)
    )
    (let (
            (policy (unwrap! (get-policy flight-number departure-time) err-no-policy))
            (current-block burn-block-height)
        )
        (asserts! (is-eq (get owner policy) tx-sender) err-owner-only)
        (asserts! (get active policy) err-not-transferable)
        (asserts! (not (get claimed policy)) err-already-claimed)
        (asserts! (> transfer-price u0) err-invalid-transfer-price)
        (asserts! (> expires-after-blocks u0) err-invalid-transfer-price)
        (ok (map-set policy-transfers {
            flight-number: flight-number,
            departure-time: departure-time,
        } {
            seller: tx-sender,
            transfer-price: transfer-price,
            listed-at: current-block,
            expires-at: (+ current-block expires-after-blocks),
        }))
    )
)

(define-public (purchase-transferred-policy
        (flight-number (string-ascii 10))
        (departure-time uint)
    )
    (let (
            (policy (unwrap! (get-policy flight-number departure-time) err-no-policy))
            (transfer-listing (unwrap!
                (map-get? policy-transfers {
                    flight-number: flight-number,
                    departure-time: departure-time,
                })
                err-transfer-not-found
            ))
            (seller (get seller transfer-listing))
            (transfer-price (get transfer-price transfer-listing))
            (transfer-fee (/ (* transfer-price transfer-fee-percentage) u100))
            (seller-amount (- transfer-price transfer-fee))
        )
        (asserts! (not (is-eq tx-sender seller)) err-cannot-transfer-to-self)
        (asserts! (<= burn-block-height (get expires-at transfer-listing))
            err-transfer-not-found
        )
        (asserts! (get active policy) err-not-transferable)
        (asserts! (not (get claimed policy)) err-already-claimed)
        (try! (stx-transfer? seller-amount tx-sender seller))
        (try! (stx-transfer? transfer-fee tx-sender contract-owner))
        (map-delete policy-transfers {
            flight-number: flight-number,
            departure-time: departure-time,
        })
        (ok (map-set flight-policies {
            flight-number: flight-number,
            departure-time: departure-time,
        } {
            owner: tx-sender,
            delay-minutes: (get delay-minutes policy),
            claimed: false,
            active: true,
        }))
    )
)

(define-public (cancel-policy-transfer
        (flight-number (string-ascii 10))
        (departure-time uint)
    )
    (let ((transfer-listing (unwrap!
            (map-get? policy-transfers {
                flight-number: flight-number,
                departure-time: departure-time,
            })
            err-transfer-not-found
        )))
        (asserts! (is-eq tx-sender (get seller transfer-listing)) err-owner-only)
        (ok (map-delete policy-transfers {
            flight-number: flight-number,
            departure-time: departure-time,
        }))
    )
)

(define-read-only (get-policy-transfer-listing
        (flight-number (string-ascii 10))
        (departure-time uint)
    )
    (map-get? policy-transfers {
        flight-number: flight-number,
        departure-time: departure-time,
    })
)

(define-read-only (is-transfer-listing-active
        (flight-number (string-ascii 10))
        (departure-time uint)
    )
    (match (get-policy-transfer-listing flight-number departure-time)
        transfer-listing (and
            (<= burn-block-height (get expires-at transfer-listing))
            (match (get-policy flight-number departure-time)
                policy (and (get active policy) (not (get claimed policy)))
                false
            )
        )
        false
    )
)

(define-constant minimum-stake u1000000000)
(define-constant unstake-cooldown u144)
(define-constant staker-premium-share u40)

(define-data-var total-pool-balance uint u0)
(define-data-var total-staked uint u0)

(define-map stakers
    { staker: principal }
    {
        amount-staked: uint,
        last-stake-block: uint,
        rewards-earned: uint,
        unstake-requested-at: uint,
    }
)

(define-map pool-metrics
    { epoch: uint }
    {
        total-premiums-collected: uint,
        total-claims-paid: uint,
        stakers-count: uint,
    }
)

(define-data-var current-epoch uint u0)

(define-public (stake-in-pool (amount uint))
    (let (
            (existing-stake (map-get? stakers { staker: tx-sender }))
            (current-staked (if (is-some existing-stake)
                (get amount-staked (unwrap-panic existing-stake))
                u0
            ))
            (current-rewards (if (is-some existing-stake)
                (get rewards-earned (unwrap-panic existing-stake))
                u0
            ))
        )
        (asserts! (>= amount minimum-stake) err-insufficient-stake)
        (try! (stx-transfer? amount tx-sender (as-contract tx-sender)))
        (var-set total-staked (+ (var-get total-staked) amount))
        (var-set total-pool-balance (+ (var-get total-pool-balance) amount))
        (ok (map-set stakers { staker: tx-sender } {
            amount-staked: (+ current-staked amount),
            last-stake-block: burn-block-height,
            rewards-earned: current-rewards,
            unstake-requested-at: u0,
        }))
    )
)

(define-public (request-unstake)
    (let ((stake-data (unwrap! (map-get? stakers { staker: tx-sender }) err-no-stake-found)))
        (asserts! (> (get amount-staked stake-data) u0) err-insufficient-stake)
        (asserts! (is-eq (get unstake-requested-at stake-data) u0)
            err-cooldown-active
        )
        (ok (map-set stakers { staker: tx-sender } {
            amount-staked: (get amount-staked stake-data),
            last-stake-block: (get last-stake-block stake-data),
            rewards-earned: (get rewards-earned stake-data),
            unstake-requested-at: burn-block-height,
        }))
    )
)

(define-public (execute-unstake)
    (let (
            (stake-data (unwrap! (map-get? stakers { staker: tx-sender }) err-no-stake-found))
            (requested-at (get unstake-requested-at stake-data))
            (staked-amount (get amount-staked stake-data))
            (rewards (get rewards-earned stake-data))
            (total-withdrawal (+ staked-amount rewards))
        )
        (asserts! (> requested-at u0) err-cooldown-active)
        (asserts! (>= burn-block-height (+ requested-at unstake-cooldown))
            err-cooldown-active
        )
        (asserts! (>= (var-get total-pool-balance) total-withdrawal)
            err-insufficient-pool-balance
        )
        (try! (as-contract (stx-transfer? total-withdrawal tx-sender tx-sender)))
        (var-set total-staked (- (var-get total-staked) staked-amount))
        (var-set total-pool-balance
            (- (var-get total-pool-balance) total-withdrawal)
        )
        (ok (map-delete stakers { staker: tx-sender }))
    )
)

(define-public (distribute-premium-to-stakers (premium-amount uint))
    (let (
            (staker-share (/ (* premium-amount staker-premium-share) u100))
            (pool-share (- premium-amount staker-share))
        )
        (var-set total-pool-balance (+ (var-get total-pool-balance) pool-share))
        (ok staker-share)
    )
)

(define-public (claim-staker-rewards)
    (let (
            (stake-data (unwrap! (map-get? stakers { staker: tx-sender }) err-no-stake-found))
            (rewards (get rewards-earned stake-data))
        )
        (asserts! (> rewards u0) err-insufficient-stake)
        (asserts! (>= (var-get total-pool-balance) rewards)
            err-insufficient-pool-balance
        )
        (try! (as-contract (stx-transfer? rewards tx-sender tx-sender)))
        (var-set total-pool-balance (- (var-get total-pool-balance) rewards))
        (ok (map-set stakers { staker: tx-sender } {
            amount-staked: (get amount-staked stake-data),
            last-stake-block: (get last-stake-block stake-data),
            rewards-earned: u0,
            unstake-requested-at: (get unstake-requested-at stake-data),
        }))
    )
)

(define-public (allocate-rewards-to-staker
        (staker principal)
        (reward-amount uint)
    )
    (let ((stake-data (unwrap! (map-get? stakers { staker: staker }) err-no-stake-found)))
        (ok (map-set stakers { staker: staker } {
            amount-staked: (get amount-staked stake-data),
            last-stake-block: (get last-stake-block stake-data),
            rewards-earned: (+ (get rewards-earned stake-data) reward-amount),
            unstake-requested-at: (get unstake-requested-at stake-data),
        }))
    )
)

(define-read-only (get-staker-info (staker principal))
    (map-get? stakers { staker: staker })
)

(define-read-only (get-pool-stats)
    {
        total-pool-balance: (var-get total-pool-balance),
        total-staked: (var-get total-staked),
        current-epoch: (var-get current-epoch),
    }
)

(define-read-only (calculate-staker-share (staker principal))
    (match (map-get? stakers { staker: staker })
        stake-data (if (> (var-get total-staked) u0)
            (/ (* (get amount-staked stake-data) u10000) (var-get total-staked))
            u0
        )
        u0
    )
)

(define-read-only (is-unstake-ready (staker principal))
    (match (map-get? stakers { staker: staker })
        stake-data (let ((requested-at (get unstake-requested-at stake-data)))
            (and
                (> requested-at u0)
                (>= burn-block-height (+ requested-at unstake-cooldown))
            )
        )
        false
    )
)
