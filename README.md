# Crowdsourced Local Guide DAO
A decentralized platform where locals can earn tokens by sharing authentic tourism information.

## 🎯 Features

- Create and share local tourism spots
- Earn tokens for verified contributions
- Vote on locations using tokens
- Community-driven verification system

## 🔧 Smart Contract Functions

### Core Functions

- `add-location`: Add a new tourism location
- `verify-location`: Verify a location (DAO owner only)
- `vote-location`: Vote on a location's authenticity
- `transfer-tokens`: Transfer tokens between users

### Read-Only Functions

- `get-location`: Get location details
- `get-user-votes`: Check user's voting history
- `get-user-rewards`: View user rewards
- `get-balance`: Check token balance

## 💎 Token Economics

- 10 tokens for adding a location
- 50 tokens when location gets verified
- 5 tokens for voting on locations
- Minimum 100 tokens required to vote

## 🚀 Getting Started

1. Deploy the contract using Clarinet
2. Initialize the DAO owner
3. Start adding locations and earning tokens

## 📝 Usage Example

```clarity
;; Add a new location
(contract-call? .crowdsourced-local-guide-dao add-location 
    u1 
    "Eiffel Tower" 
    "Famous landmark in Paris" 
    48859 
    2351)
```

## 🔐 Security

- Only DAO owner can verify locations
- Double-voting prevention
- Token-gated voting system
```
