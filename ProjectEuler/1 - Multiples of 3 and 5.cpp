#include <map>
#include <set>
#include <list>
#include <cmath>
#include <ctime>
#include <deque>
#include <queue>
#include <stack>
#include <string>
#include <bitset>
#include <cstdio>
#include <limits>
#include <vector>
#include <climits>
#include <cstring>
#include <cstdlib>
#include <fstream>
#include <numeric>
#include <sstream>
#include <iostream>
#include <algorithm>
#include <unordered_map>

using namespace std;

int main(){
    int t, threes, fives, fifteens;
    cin >> t;
    for(int a0 = 0; a0 < t; a0++){
        int n;
        cin >> n;
        n--;
        unsigned long int div3 = n/3; div3=div3*(div3+1)/2;
        unsigned long int div5 = n/5; div5=div5*(div5+1)/2;
        unsigned long int div15 = n/15; div15=div15*(div15+1)/2;
        printf("%ld\n",div3*3+div5*5-div15*15);
    }

    
    return 0;
}
