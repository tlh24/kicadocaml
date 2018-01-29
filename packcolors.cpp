#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <time.h>
#include <math.h>

#define NN 21
float best[NN][3]; 
float bestd; 
float uniform(){
	return (float)(rand()) / (float)RAND_MAX ; 
}
float 			g_normal; 
bool 				g_normal_ok = false; 
float normal(){
	if(g_normal_ok){
		g_normal_ok = false; 
		return g_normal; 
	}else{
		//generate two normals via box-muller transform.
		float s = 3.0; 
		float v1,v2;
		while(s >= 1.0){ //get a vector within the unit circle?
			float u1 = uniform(); 
			float u2 = uniform(); 
			v1 = 2*u1 - 1; 
			v2 = 2*u2 - 1; 
			s = v1*v1 + v2*v2; 
		}
		float x = sqrt(-2*log(s)/s)*v1; 
		float y = sqrt(-2*log(s)/s)*v2; 
		g_normal_ok = true; 
		g_normal = x; 
		return y; 
	}
}
int main(void){
  time_t t;
  srand((unsigned) time(&t));
  float p[NN][3]; 
  bestd = 0.0; 
  int n=0; 
  while(1){
    if((n%10) == 0){
      for(int j=0; j<NN; j++){
        int ok = 0; 
        while(ok==0){
          float len = 0; 
          for(int i=0; i<3; i++){
            p[j][i] = uniform(); 
            len += p[j][i] * p[j][i]; 
          }
          len = sqrt(len); 
          if(len > 0.4 && len < 1.3) ok = 1; 
        }
      }
    } else {
      for(int j=0; j<NN; j++){
        int ok = 0; 
        while(ok==0){
          float len = 0; 
          for(int i=0; i<3; i++){
            p[j][i] = best[j][i] + normal() * (0.013 * ((n%10)+1)); 
            p[j][i] = p[j][i] > 1.0 ? 1.0 : (p[j][i] < 0.0 ? 0.0 : p[j][i]); 
            len += p[j][i] * p[j][i]; 
          }
          len = sqrt(len); 
          if(len > 0.4 && len < 1.3) ok = 1; 
        }
      }
    }
    float min = 1e6; 
    for(int j=0; j<NN-1; j++){
      for(int i=j+1; i<NN; i++){
        float dist = (p[j][0]-p[i][0])*(p[j][0]-p[i][0]) + 
          (p[j][1]-p[i][1])*(p[j][1]-p[i][1]) + 
          (p[j][2]-p[i][2])*(p[j][2]-p[i][2]); 
        if(dist < min) min = dist; 
      }
    }
    if(min > bestd){
      bestd = min; 
      printf("----- maximizing min dist %f\n", bestd); 
      for(int j=0; j<NN; j++){
        printf(" -> ("); 
        for(int i=0; i<3; i++){
          best[j][i] = p[j][i];
          printf("%.2f", p[j][i]); 
          if(i<2) printf(", "); 
        }
        printf(");\n"); 
      }
    }
    n++;
  }
}
