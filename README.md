# Decentralized Flight Delay Insurance
A trustless flight delay insurance platform built on Stacks blockchain that automatically pays out claims based on verified flight delay data.

## 🎯 Features

- Purchase flight delay insurance policies
- Oracle-powered flight delay verification
- Automatic claim processing
- Policy management and cancellation
- Transparent and immutable contract terms

## 💡 How it Works

1. Users purchase insurance for specific flights
2. Oracle feeds real flight data to the contract
3. If delay exceeds 120 minutes, policy becomes claimable
4. Verified claims trigger automatic STX payouts

## 🔧 Contract Functions

### User Functions
- `purchase-policy`: Buy insurance for a flight
- `claim-insurance`: Claim payout for delayed flight
- `cancel-policy`: Cancel policy for 50% refund
- `get-policy`: View policy details

### Oracle Functions
- `update-flight-data`: Update flight status
- `record-delay`: Record flight delays
- `set-oracle-address`: Update oracle address

## 💰 Economics

- Policy Price: 100 STX
- Payout Amount: 300 STX
- Minimum Delay: 120 minutes

## 🚀 Getting Started

1. Deploy contract using Clarinet
2. Purchase policy with flight details
3. Wait for oracle to confirm delays
4. Claim insurance if eligible

## ⚠️ Requirements

- Stacks 2.0
- Clarinet
- STX tokens for policy purchase
```
