(define-constant ERR-NOT-AUTHORIZED (err u100))
(define-constant ERR-INVALID-LOCATION (err u101))
(define-constant ERR-ALREADY-VERIFIED (err u102))
(define-constant ERR-NOT-FOUND (err u103))
(define-constant ERR-INSUFFICIENT-TOKENS (err u104))

(define-fungible-token local-guide-token)

(define-data-var token-uri (string-utf8 256) u"")
(define-data-var dao-owner principal tx-sender)
(define-data-var min-tokens-to-vote uint u100)

(define-map locations
    { location-id: uint }
    {
        creator: principal,
        title: (string-utf8 100),
        description: (string-utf8 500),
        latitude: int,
        longitude: int,
        verified: bool,
        votes: uint,
        created-at: uint,
    }
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

(define-read-only (get-location (location-id uint))
    (map-get? locations { location-id: location-id })
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

(define-read-only (get-balance (account principal))
    (ft-get-balance local-guide-token account)
)

(define-public (set-token-uri (new-uri (string-utf8 256)))
    (begin
        (asserts! (is-eq tx-sender (var-get dao-owner)) ERR-NOT-AUTHORIZED)
        (ok (var-set token-uri new-uri))
    )
)

(define-public (add-location
        (location-id uint)
        (title (string-utf8 100))
        (description (string-utf8 500))
        (latitude int)
        (longitude int)
    )
    (let ((location (get-location location-id)))
        (asserts! (is-none location) ERR-ALREADY-VERIFIED)
        (map-set locations { location-id: location-id } {
            creator: tx-sender,
            title: title,
            description: description,
            latitude: latitude,
            longitude: longitude,
            verified: false,
            votes: u0,
            created-at: burn-block-height,
        })
        (try! (mint-tokens tx-sender u10))
        (ok true)
    )
)

(define-public (verify-location (location-id uint))
    (let ((location (unwrap! (get-location location-id) ERR-NOT-FOUND)))
        (asserts! (is-eq tx-sender (var-get dao-owner)) ERR-NOT-AUTHORIZED)
        (map-set locations { location-id: location-id }
            (merge location { verified: true })
        )
        (try! (mint-tokens (get creator location) u50))
        (ok true)
    )
)

(define-public (vote-location (location-id uint))
    (let (
            (user-vote (get-user-votes tx-sender location-id))
            (location (unwrap! (get-location location-id) ERR-NOT-FOUND))
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
        (try! (mint-tokens tx-sender u5))
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
