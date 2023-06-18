let fib_memo = fn(x, memo) {
    if (memo[x]) {
        return memo[x];
    }

    if (x < 2) {
        return x;
    } else {
        let memo = add(memo, x - 2, fib_memo(x - 2, memo))
        let memo = add(memo, x - 1, fib_memo(x - 1, memo))

        return memo[x - 2] + memo[x - 1];
    }
}

let fib = fn(x) {
    if (x < 2) {
        return x;
    } else {
        return fib(x - 2) + fib(x - 1);
    }
}

puts(fib(35))

