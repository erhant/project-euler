/*
topics:
  - array
  - dynamic programming
difficulty: easy
*/

int maxProfit(int* prices, int pricesSize) {
  int dip = INT_MAX;
  int cur_profit, max_profit = 0;
  for (int i = 0; i < pricesSize; ++i) {
    // we have a new dip
    if (prices[i] < dip) {
      dip = prices[i];
    }
    // sell today
    cur_profit = prices[i] - dip;
    // is it more profit?
    if (cur_profit > max_profit) {
      max_profit = cur_profit;
    }
  }
  return max_profit;
}