(define-constant ERR-NOT-AUTHORIZED (err u100))
(define-constant ERR-INVALID-LOCATION (err u101))
(define-constant ERR-ALREADY-VERIFIED (err u102))
(define-constant ERR-NOT-FOUND (err u103))
(define-constant ERR-INSUFFICIENT-TOKENS (err u104))
(define-constant ERR-INVALID-CATEGORY (err u105))
(define-constant ERR-INSUFFICIENT-REPUTATION (err u106))
(define-constant ERR-INVALID-RATING (err u107))
(define-constant ERR-ALREADY-RATED (err u108))

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
