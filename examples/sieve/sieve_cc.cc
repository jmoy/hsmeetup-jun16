#include <iostream>
#include <vector>
#include <cstdlib>
using namespace std;

long count_primes(const long);

int main(int argc,char *argv[])
{
  if (argc!=2){
    cerr<<"Usage: sieve_cc [n]\n";
    return 1;
  }
  long N = atol(argv[1]);
  cout<<"Count = "<<count_primes(N)<<'\n';
}

long count_primes(const long n){
//  vector<bool> mask(n+1,true);
  vector<char> mask(n,true);
  mask[0]=mask[1]=false;
  for (long p=2;p<n;p++){
    if (mask[p]){
      for (long j=p*p;j<n;j+=p){
        mask[j] = false;
      }
    }
  }
  long count = 0;
  for (auto m:mask){
    if (m){
      count++;
    }
  }
  return count;
}

