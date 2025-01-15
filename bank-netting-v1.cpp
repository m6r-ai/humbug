#include <iostream>
#include <fstream>
#include <sstream>
#include <vector>
#include <unordered_map>
#include <algorithm>

using namespace std;

struct Transaction {
    string payer;
    string payee;
    double amount;
};

struct Settlement {
    string payer;
    string payee;
    double amount;
};

// Function to read initial balances from CSV file
unordered_map<string, double> readInitialBalances(const string& filename) {
    unordered_map<string, double> initialBalances;
    ifstream file(filename);
    string line, accountHolder;
    double initialBalance;
    
    // Skip header
    getline(file, line);
    
    while (getline(file, line)) {
        stringstream ss(line);
        getline(ss, accountHolder, ',');
        ss >> initialBalance;
        initialBalances[accountHolder] = initialBalance;
    }
    return initialBalances;
}

// Function to read transactions from CSV file
vector<Transaction> readTransactions(const string& filename) {
    vector<Transaction> transactions;
    ifstream file(filename);
    string line, payer, payee;
    double amount;
    
    // Skip header
    getline(file, line);
    
    while (getline(file, line)) {
        stringstream ss(line);
        getline(ss, payer, ',');
        getline(ss, payee, ',');
        ss >> amount;
        transactions.push_back({payer, payee, amount});
    }
    return transactions;
}

// Function to calculate net balances after all transactions
unordered_map<string, double> calculateNetBalances(const unordered_map<string, double>& initialBalances, const vector<Transaction>& transactions) {
    unordered_map<string, double> netBalances = initialBalances;
    
    for (const auto& transaction : transactions) {
        netBalances[transaction.payer] -= transaction.amount;
        netBalances[transaction.payee] += transaction.amount;
    }
    return netBalances;
}

// Function to calculate minimal settlements
vector<Settlement> calculateSettlements(const unordered_map<string, double>& initialBalances, const unordered_map<string, double>& netBalances) {
    vector<Settlement> settlements;
    vector<pair<string, double>> creditors, debtors;
    
    for (const auto& entry : netBalances) {
        double difference = entry.second - initialBalances.at(entry.first);
        if (difference > 0) {
            creditors.push_back({entry.first, difference});
        } else if (difference < 0) {
            debtors.push_back({entry.first, -difference});
        }
    }

    // Sorting creditors and debtors is not strictly necessary but can help make the process more systematic
    sort(creditors.begin(), creditors.end(), [](const pair<string, double>& a, const pair<string, double>& b) {
        return a.second > b.second;
    });
    sort(debtors.begin(), debtors.end(), [](const pair<string, double>& a, const pair<string, double>& b) {
        return a.second > b.second;
    });

    int credIndex = 0, debtIndex = 0;
    while (credIndex < creditors.size() && debtIndex < debtors.size()) {
        double settleAmount = min(creditors[credIndex].second, debtors[debtIndex].second);
        settlements.push_back({debtors[debtIndex].first, creditors[credIndex].first, settleAmount});
        creditors[credIndex].second -= settleAmount;
        debtors[debtIndex].second -= settleAmount;
        
        if (creditors[credIndex].second == 0) credIndex++;
        if (debtors[debtIndex].second == 0) debtIndex++;
    }
    return settlements;
}

// Function to write settlements to CSV file
void writeSettlements(const string& filename, const vector<Settlement>& settlements) {
    ofstream file(filename);
    file << "Payer,Payee,Amount\n";
    for (const auto& settlement : settlements) {
        file << settlement.payer << "," << settlement.payee << "," << settlement.amount << "\n";
    }
}

int main(int argc, char* argv[]) {
    if (argc != 4) {
        cerr << "Usage: " << argv[0] << " <initial_balances.csv> <transactions.csv> <settlements.csv>\n";
        return 1;
    }
    
    string initialBalancesFile = argv[1];
    string transactionsFile = argv[2];
    string settlementsFile = argv[3];
    
    // Read initial balances and transactions
    auto initialBalances = readInitialBalances(initialBalancesFile);
    auto transactions = readTransactions(transactionsFile);
    
    // Debugging: Print initial balances
    cout << "Initial Balances:" << endl;
    for (const auto& balance : initialBalances) {
        cout << balance.first << ": " << balance.second << endl;
    }
    
    // Debugging: Print transactions
    cout << "\nTransactions:" << endl;
    for (const auto& transaction : transactions) {
        cout << transaction.payer << " -> " << transaction.payee << ": " << transaction.amount << endl;
    }
    
    // Calculate net balances
    auto netBalances = calculateNetBalances(initialBalances, transactions);
    
    // Debugging: Print net balances
    cout << "\nNet Balances:" << endl;
    for (const auto& balance : netBalances) {
        cout << balance.first << ": " << balance.second << endl;
    }
    
    // Calculate settlements
    auto settlements = calculateSettlements(initialBalances, netBalances);
    
    // Debugging: Print settlements
    cout << "\nSettlements:" << endl;
    for (const auto& settlement : settlements) {
        cout << settlement.payer << " -> " << settlement.payee << ": " << settlement.amount << endl;
    }
    
    // Write settlements to CSV
    writeSettlements(settlementsFile, settlements);
    
    cout << "Settlements have been calculated and written to " << settlementsFile << endl;
    return 0;
}

