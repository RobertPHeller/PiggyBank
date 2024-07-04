# PiggyBank
Simple COBOL programs implementing a simple children's bank.

There are two programs:

1. PiggyBank - This is a bank teller interface.
1. TransReport - This is a transaction report generating program.

Two datafiles are used:

1. Accounts.dat This contains the names and current balances (in Pennies) 
    for each customer of Piggy Bank.
    Each record has two fields: a 16 character customer name and a signed 
    8 digit balance (Pennies)
1. Transactions.dat This contains the transaction records for Piggy Bank.
    Each transaction record contains a 32 character Transaction ID (actually
    a timestamp of the transaction, a two digit account number, and a signed
    8 digit transaction ammount (in Pennies).
