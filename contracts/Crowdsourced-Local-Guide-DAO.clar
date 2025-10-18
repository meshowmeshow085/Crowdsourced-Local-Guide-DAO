(define-constant ERR-NOT-AUTHORIZED (err u100))
(define-constant ERR-INVALID-LOCATION (err u101))
(define-constant ERR-ALREADY-VERIFIED (err u102))
(define-constant ERR-NOT-FOUND (err u103))
(define-constant ERR-INSUFFICIENT-TOKENS (err u104))
(define-constant ERR-INVALID-CATEGORY (err u105))
(define-constant ERR-INSUFFICIENT-REPUTATION (err u106))
(define-constant ERR-INVALID-RATING (err u107))
(define-constant ERR-ALREADY-RATED (err u108))
(define-constant ERR-CANNOT-TIP-SELF (err u109))
(define-constant ERR-INVALID-TIP-AMOUNT (err u110))

(define-fungible-token local-guide-token)

(define-data-var token-uri (string-utf8 256) u"")
(define-data-var dao-owner principal tx-sender)
(define-data-var min-tokens-to-vote uint u100)
(define-data-var next-location-id uint u1)

(define-map locations
    { location-id: uint }
    {
        creator: principal,
        title: (string-utf8 100),
        description: (string-utf8 500),
        latitude: int,
        longitude: int,
        category: uint,
        verified: bool,
        votes: uint,
        rating-sum: uint,
        rating-count: uint,
        avg-rating: uint,
        created-at: uint,
    }
)

(define-map categories
    { category-id: uint }
    {
        name: (string-utf8 50),
        description: (string-utf8 200),
        active: bool,
    }
)

(define-map category-locations
    {
        category-id: uint,
        location-id: uint,
    }
    { exists: bool }
)

(define-map user-votes
    {
        user: principal,
        location-id: uint,
    }
    { voted: bool }
)

(define-map user-rewards
    { user: principal }
    { total-earned: uint }
)

(define-map user-reputation
    { user: principal }
    {
        points: uint,
        level: uint,
        locations-added: uint,
        locations-verified: uint,
        votes-received: uint,
        last-updated: uint,
    }
)

(define-map location-ratings
    {
        user: principal,
        location-id: uint,
    }
    {
        rating: uint,
        weight: uint,
        created-at: uint,
    }
)

(define-map reputation-levels
    { level: uint }
    {
        name: (string-utf8 50),
        min-points: uint,
        token-multiplier: uint,
        can-verify: bool,
    }
)

(define-map location-tips
    { location-id: uint }
    {
        total-tips: uint,
        tip-count: uint,
    }
)

(define-map user-tip-stats
    { user: principal }
    {
        tips-sent: uint,
        tips-received: uint,
        total-sent-amount: uint,
        total-received-amount: uint,
    }
)

(define-map tip-history
    {
        tipper: principal,
        location-id: uint,
    }
    {
        total-amount: uint,
        tip-count: uint,
        last-tip-at: uint,
    }
)

(define-read-only (get-location (location-id uint))
    (map-get? locations { location-id: location-id })
)

(define-read-only (get-category (category-id uint))
    (map-get? categories { category-id: category-id })
)

(define-read-only (get-locations-by-category (category-id uint))
    (let ((category (unwrap! (get-category category-id) ERR-NOT-FOUND)))
        (asserts! (get active category) ERR-INVALID-CATEGORY)
        (ok category-id)
    )
)

(define-read-only (get-user-votes
        (user principal)
        (location-id uint)
    )
    (map-get? user-votes {
        user: user,
        location-id: location-id,
    })
)

(define-read-only (get-user-rewards (user principal))
    (map-get? user-rewards { user: user })
)

(define-read-only (get-user-reputation (user principal))
    (map-get? user-reputation { user: user })
)

(define-read-only (get-reputation-level (level uint))
    (map-get? reputation-levels { level: level })
)

(define-read-only (get-balance (account principal))
    (ft-get-balance local-guide-token account)
)

(define-read-only (get-location-rating
        (user principal)
        (location-id uint)
    )
    (map-get? location-ratings {
        user: user,
        location-id: location-id,
    })
)

(define-read-only (get-location-tips (location-id uint))
    (map-get? location-tips { location-id: location-id })
)

(define-read-only (get-user-tip-stats (user principal))
    (map-get? user-tip-stats { user: user })
)

(define-read-only (get-tip-history
        (tipper principal)
        (location-id uint)
    )
    (map-get? tip-history {
        tipper: tipper,
        location-id: location-id,
    })
)

(define-private (abs-int (value int))
    (if (< value 0)
        (* value -1)
        value
    )
)

(define-read-only (calculate-distance
        (lat1 int)
        (lon1 int)
        (lat2 int)
        (lon2 int)
    )
    (let (
            (dlat (abs-int (- lat2 lat1)))
            (dlon (abs-int (- lon2 lon1)))
            (distance-squared (+ (* dlat dlat) (* dlon dlon)))
        )
        distance-squared
    )
)

(define-read-only (is-location-within-radius
        (center-lat int)
        (center-lon int)
        (target-lat int)
        (target-lon int)
        (radius-squared uint)
    )
    (let ((distance-sq (calculate-distance center-lat center-lon target-lat target-lon)))
        (<= (to-uint distance-sq) radius-squared)
    )
)

(define-read-only (discover-locations-in-radius
        (center-lat int)
        (center-lon int)
        (radius-squared uint)
        (start-id uint)
        (limit uint)
    )
    (fold check-location-in-radius
        (list
            start-id             (+ start-id u1)
            (+ start-id u2)             (+ start-id u3)
            (+ start-id u4)
            (+ start-id u5)             (+ start-id u6)             (+ start-id u7)
            (+ start-id u8)             (+ start-id u9)
        ) {
        center-lat: center-lat,
        center-lon: center-lon,
        radius-squared: radius-squared,
        results: (list),
        count: u0,
        limit: limit,
    })
)

(define-private (check-location-in-radius
        (location-id uint)
        (acc {
            center-lat: int,
            center-lon: int,
            radius-squared: uint,
            results: (list 10 uint),
            count: uint,
            limit: uint,
        })
    )
    (let ((location-opt (get-location location-id)))
        (if (and
                (< (get count acc) (get limit acc))
                (is-some location-opt)
            )
            (let (
                    (location (unwrap-panic location-opt))
                    (within-radius (is-location-within-radius (get center-lat acc)
                        (get center-lon acc) (get latitude location)
                        (get longitude location) (get radius-squared acc)
                    ))
                )
                (if within-radius
                    (merge acc {
                        results: (unwrap-panic (as-max-len? (append (get results acc) location-id) u10)),
                        count: (+ (get count acc) u1),
                    })
                    acc
                )
            )
            acc
        )
    )
)

(define-read-only (discover-verified-locations
        (center-lat int)
        (center-lon int)
        (radius-squared uint)
        (min-rating uint)
        (start-id uint)
        (limit uint)
    )
    (fold check-verified-location-in-radius
        (list
            start-id             (+ start-id u1)
            (+ start-id u2)             (+ start-id u3)
            (+ start-id u4)
            (+ start-id u5)             (+ start-id u6)
            (+ start-id u7)
            (+ start-id u8)             (+ start-id u9)
        ) {
        center-lat: center-lat,
        center-lon: center-lon,
        radius-squared: radius-squared,
        min-rating: min-rating,
        results: (list),
        count: u0,
        limit: limit,
    })
)

(define-private (check-verified-location-in-radius
        (location-id uint)
        (acc {
            center-lat: int,
            center-lon: int,
            radius-squared: uint,
            min-rating: uint,
            results: (list 10 uint),
            count: uint,
            limit: uint,
        })
    )
    (let ((location-opt (get-location location-id)))
        (if (and
                (< (get count acc) (get limit acc))
                (is-some location-opt)
            )
            (let (
                    (location (unwrap-panic location-opt))
                    (within-radius (is-location-within-radius (get center-lat acc)
                        (get center-lon acc) (get latitude location)
                        (get longitude location) (get radius-squared acc)
                    ))
                    (meets-criteria (and
                        (get verified location)
                        (>= (get avg-rating location) (get min-rating acc))
                    ))
                )
                (if (and within-radius meets-criteria)
                    (merge acc {
                        results: (unwrap-panic (as-max-len? (append (get results acc) location-id) u10)),
                        count: (+ (get count acc) u1),
                    })
                    acc
                )
            )
            acc
        )
    )
)

(define-private (get-user-level (user principal))
    (let ((reputation (get-user-reputation user)))
        (if (is-some reputation)
            (get level (unwrap-panic reputation))
            u0
        )
    )
)

(define-private (calculate-level (points uint))
    (if (>= points u1000)
        u4
        (if (>= points u500)
            u3
            (if (>= points u100)
                u2
                (if (>= points u25)
                    u1
                    u0
                )
            )
        )
    )
)

(define-private (update-user-reputation
        (user principal)
        (points-to-add uint)
        (locations-added-increment uint)
        (locations-verified-increment uint)
        (votes-received-increment uint)
    )
    (let (
            (current-rep (default-to {
                points: u0,
                level: u0,
                locations-added: u0,
                locations-verified: u0,
                votes-received: u0,
                last-updated: u0,
            }
                (get-user-reputation user)
            ))
            (new-points (+ (get points current-rep) points-to-add))
            (new-level (calculate-level new-points))
        )
        (map-set user-reputation { user: user } {
            points: new-points,
            level: new-level,
            locations-added: (+ (get locations-added current-rep) locations-added-increment),
            locations-verified: (+ (get locations-verified current-rep) locations-verified-increment),
            votes-received: (+ (get votes-received current-rep) votes-received-increment),
            last-updated: burn-block-height,
        })
        (ok new-level)
    )
)

(define-public (set-token-uri (new-uri (string-utf8 256)))
    (begin
        (asserts! (is-eq tx-sender (var-get dao-owner)) ERR-NOT-AUTHORIZED)
        (ok (var-set token-uri new-uri))
    )
)

(define-public (initialize-reputation-levels)
    (begin
        (asserts! (is-eq tx-sender (var-get dao-owner)) ERR-NOT-AUTHORIZED)
        (map-set reputation-levels { level: u0 } {
            name: u"Newcomer",
            min-points: u0,
            token-multiplier: u1,
            can-verify: false,
        })
        (map-set reputation-levels { level: u1 } {
            name: u"Explorer",
            min-points: u25,
            token-multiplier: u2,
            can-verify: false,
        })
        (map-set reputation-levels { level: u2 } {
            name: u"Guide",
            min-points: u100,
            token-multiplier: u3,
            can-verify: false,
        })
        (map-set reputation-levels { level: u3 } {
            name: u"Expert",
            min-points: u500,
            token-multiplier: u4,
            can-verify: true,
        })
        (map-set reputation-levels { level: u4 } {
            name: u"Master",
            min-points: u1000,
            token-multiplier: u5,
            can-verify: true,
        })
        (ok true)
    )
)

(define-public (add-category
        (category-id uint)
        (name (string-utf8 50))
        (description (string-utf8 200))
    )
    (begin
        (asserts! (is-eq tx-sender (var-get dao-owner)) ERR-NOT-AUTHORIZED)
        (asserts! (is-none (get-category category-id)) ERR-ALREADY-VERIFIED)
        (map-set categories { category-id: category-id } {
            name: name,
            description: description,
            active: true,
        })
        (ok true)
    )
)

(define-public (add-location
        (title (string-utf8 100))
        (description (string-utf8 500))
        (latitude int)
        (longitude int)
        (category-id uint)
    )
    (let (
            (location-id (var-get next-location-id))
            (category (unwrap! (get-category category-id) ERR-INVALID-CATEGORY))
            (user-level (get-user-level tx-sender))
            (level-info (unwrap! (get-reputation-level user-level) ERR-NOT-FOUND))
            (token-reward (* u10 (get token-multiplier level-info)))
        )
        (asserts! (get active category) ERR-INVALID-CATEGORY)
        (map-set locations { location-id: location-id } {
            creator: tx-sender,
            title: title,
            description: description,
            latitude: latitude,
            longitude: longitude,
            category: category-id,
            verified: false,
            votes: u0,
            rating-sum: u0,
            rating-count: u0,
            avg-rating: u0,
            created-at: burn-block-height,
        })
        (map-set category-locations {
            category-id: category-id,
            location-id: location-id,
        } { exists: true }
        )
        (begin
            (var-set next-location-id (+ location-id u1))
            (unwrap! (update-user-reputation tx-sender u10 u1 u0 u0)
                ERR-NOT-AUTHORIZED
            )
            (unwrap! (mint-tokens tx-sender token-reward) ERR-NOT-AUTHORIZED)
            (ok location-id)
        )
    )
)

(define-public (verify-location (location-id uint))
    (let (
            (location (unwrap! (get-location location-id) ERR-NOT-FOUND))
            (verifier-level (get-user-level tx-sender))
            (level-info (unwrap! (get-reputation-level verifier-level) ERR-NOT-FOUND))
            (creator (get creator location))
            (creator-level (get-user-level creator))
            (creator-level-info (unwrap! (get-reputation-level creator-level) ERR-NOT-FOUND))
            (creator-token-reward (* u50 (get token-multiplier creator-level-info)))
        )
        (asserts!
            (or (is-eq tx-sender (var-get dao-owner)) (get can-verify level-info))
            ERR-INSUFFICIENT-REPUTATION
        )
        (asserts! (not (get verified location)) ERR-ALREADY-VERIFIED)
        (map-set locations { location-id: location-id }
            (merge location { verified: true })
        )
        (unwrap! (update-user-reputation creator u25 u0 u1 u0) ERR-NOT-AUTHORIZED)
        (unwrap! (mint-tokens creator creator-token-reward) ERR-NOT-AUTHORIZED)
        (ok true)
    )
)

(define-public (vote-location (location-id uint))
    (let (
            (user-vote (get-user-votes tx-sender location-id))
            (location (unwrap! (get-location location-id) ERR-NOT-FOUND))
            (user-level (get-user-level tx-sender))
            (level-info (unwrap! (get-reputation-level user-level) ERR-NOT-FOUND))
            (token-reward (* u5 (get token-multiplier level-info)))
            (creator (get creator location))
        )
        (asserts! (is-none user-vote) ERR-ALREADY-VERIFIED)
        (asserts! (>= (get-balance tx-sender) (var-get min-tokens-to-vote))
            ERR-INSUFFICIENT-TOKENS
        )
        (map-set user-votes {
            user: tx-sender,
            location-id: location-id,
        } { voted: true }
        )
        (map-set locations { location-id: location-id }
            (merge location { votes: (+ (get votes location) u1) })
        )
        (unwrap! (update-user-reputation creator u5 u0 u0 u1) ERR-NOT-AUTHORIZED)
        (unwrap! (mint-tokens tx-sender token-reward) ERR-NOT-AUTHORIZED)
        (ok true)
    )
)

(define-private (mint-tokens
        (recipient principal)
        (amount uint)
    )
    (ft-mint? local-guide-token amount recipient)
)

(define-private (burn-tokens
        (sender principal)
        (amount uint)
    )
    (ft-burn? local-guide-token amount sender)
)

(define-public (transfer-tokens
        (recipient principal)
        (amount uint)
    )
    (ft-transfer? local-guide-token amount tx-sender recipient)
)

(define-public (rate-location
        (location-id uint)
        (rating uint)
    )
    (let (
            (location (unwrap! (get-location location-id) ERR-NOT-FOUND))
            (existing-rating (get-location-rating tx-sender location-id))
            (user-level (get-user-level tx-sender))
            (level-info (unwrap! (get-reputation-level user-level) ERR-NOT-FOUND))
            (weight (+ u1 user-level))
            (weighted-rating (* rating weight))
            (current-sum (get rating-sum location))
            (current-count (get rating-count location))
        )
        (asserts! (and (>= rating u1) (<= rating u5)) ERR-INVALID-RATING)
        (asserts! (is-none existing-rating) ERR-ALREADY-RATED)
        (asserts! (>= (get-balance tx-sender) (var-get min-tokens-to-vote))
            ERR-INSUFFICIENT-TOKENS
        )
        (map-set location-ratings {
            user: tx-sender,
            location-id: location-id,
        } {
            rating: rating,
            weight: weight,
            created-at: burn-block-height,
        })
        (let (
                (new-sum (+ current-sum weighted-rating))
                (new-count (+ current-count weight))
                (new-avg (/ new-sum new-count))
            )
            (map-set locations { location-id: location-id }
                (merge location {
                    rating-sum: new-sum,
                    rating-count: new-count,
                    avg-rating: new-avg,
                })
            )
            (unwrap! (update-user-reputation tx-sender u3 u0 u0 u0)
                ERR-NOT-AUTHORIZED
            )
            (unwrap!
                (mint-tokens tx-sender (* u3 (get token-multiplier level-info)))
                ERR-NOT-AUTHORIZED
            )
            (ok true)
        )
    )
)

(define-public (tip-location
        (location-id uint)
        (amount uint)
    )
    (let (
            (location (unwrap! (get-location location-id) ERR-NOT-FOUND))
            (creator (get creator location))
            (tipper-stats (default-to {
                tips-sent: u0,
                tips-received: u0,
                total-sent-amount: u0,
                total-received-amount: u0,
            }
                (get-user-tip-stats tx-sender)
            ))
            (creator-stats (default-to {
                tips-sent: u0,
                tips-received: u0,
                total-sent-amount: u0,
                total-received-amount: u0,
            }
                (get-user-tip-stats creator)
            ))
            (location-tip-data (default-to {
                total-tips: u0,
                tip-count: u0,
            }
                (get-location-tips location-id)
            ))
            (history (default-to {
                total-amount: u0,
                tip-count: u0,
                last-tip-at: u0,
            }
                (get-tip-history tx-sender location-id)
            ))
        )
        (asserts! (> amount u0) ERR-INVALID-TIP-AMOUNT)
        (asserts! (not (is-eq tx-sender creator)) ERR-CANNOT-TIP-SELF)
        (asserts! (>= (get-balance tx-sender) amount) ERR-INSUFFICIENT-TOKENS)
        (unwrap! (transfer-tokens creator amount) ERR-NOT-AUTHORIZED)
        (map-set location-tips { location-id: location-id } {
            total-tips: (+ (get total-tips location-tip-data) amount),
            tip-count: (+ (get tip-count location-tip-data) u1),
        })
        (map-set user-tip-stats { user: tx-sender } {
            tips-sent: (+ (get tips-sent tipper-stats) u1),
            tips-received: (get tips-received tipper-stats),
            total-sent-amount: (+ (get total-sent-amount tipper-stats) amount),
            total-received-amount: (get total-received-amount tipper-stats),
        })
        (map-set user-tip-stats { user: creator } {
            tips-sent: (get tips-sent creator-stats),
            tips-received: (+ (get tips-received creator-stats) u1),
            total-sent-amount: (get total-sent-amount creator-stats),
            total-received-amount: (+ (get total-received-amount creator-stats) amount),
        })
        (map-set tip-history {
            tipper: tx-sender,
            location-id: location-id,
        } {
            total-amount: (+ (get total-amount history) amount),
            tip-count: (+ (get tip-count history) u1),
            last-tip-at: burn-block-height,
        })
        (ok true)
    )
)
