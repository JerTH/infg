
struct ResourceAmount(i64); // internal representation is milligrams

struct Demand {
    consumption: ResourceAmount, // amount consumed per day
}

struct Supply {
    production: ResourceAmount, // amount produced per day
}

struct Stock {
    amount: ResourceAmount, // total amount in stock
}

struct Infrastructure {
    transportation: ResourceAmount, // total amount of materials this hub can transfer each day

}

/// An economic node represents a set of supplies and demands
struct EconomicNode {
    demands: Vec<Demand>,
    supplies: Vec<Supply>,
    inventory: Vec<Stock>,
    infrastructure: Infrastructure,
}

struct Transaction {
    tax: i64,
}
