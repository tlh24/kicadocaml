#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <time.h>
#include <math.h>

#define NN 19
#define FIXED 3
float best[NN][3]; 
float bestd; 
int layers[NN] = {15, 0, 24, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 20, 21, 22, 23}; 
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
float length(float*a){
  return sqrt(a[0]*a[0] + a[1]*a[1] + a[2]*a[2]); 
}
float dotnorm(float* a, float* b){
  float la = length(a); 
  float lb = length(b); 
  float dp = a[0]*b[0] + a[1]*b[1] + a[2]*b[2]; 
  float cost = dp / (la * lb); 
  return cost; 
}
int main(void){
  time_t t;
  srand((unsigned) time(&t));
  float p[NN][3]; 
  bestd = -1e6; 
  int n=0; 
  best[0][0] = 1; best[0][1] = 0; best[0][2] = 0; //red, metal.
  best[1][0] = 0; best[1][1] = 1; best[1][2] = 0; //green, parylene.
  best[2][0] = 0.5; best[2][1] = 0.5; best[2][2] = 0.5; //gray, outline.
  while(1){
    if((n%10) == 0){
      for(int j=0; j<FIXED; j++){
        for(int i=0; i<3; i++){
          p[j][i] = best[j][i]; 
        }
      }
      for(int j=FIXED; j<NN; j++){
        int ok = 0; 
        while(ok==0){
          float len = 0; 
          for(int i=0; i<3; i++){
            p[j][i] = uniform(); 
            len += p[j][i] * p[j][i]; 
          }
          len = sqrt(len); 
          if(len > 0.35 && len < 1.3) ok = 1; 
        }
      }
    } else {
      for(int j=0; j<FIXED; j++){
        for(int i=0; i<3; i++){
          p[j][i] = best[j][i]; 
        }
      }
      for(int j=FIXED; j<NN; j++){
        int ok = 0; 
        while(ok==0){
          float len = 0; 
          for(int i=0; i<3; i++){
            p[j][i] = best[j][i] + normal() * (0.013 * ((n%10)+1)); 
            p[j][i] = p[j][i] > 1.0 ? 1.0 : (p[j][i] < 0.0 ? 0.0 : p[j][i]); 
            len += p[j][i] * p[j][i]; 
          }
          len = sqrt(len); 
          if(len > 0.35 && len < 1.3) ok = 1; 
        }
      }
    }
    float min = 1e6; 
    for(int j=0; j<NN-1; j++){
      for(int i=j+1; i<NN; i++){
        float dist = (p[j][0]-p[i][0])*(p[j][0]-p[i][0]) + 
          (p[j][1]-p[i][1])*(p[j][1]-p[i][1]) + 
          (p[j][2]-p[i][2])*(p[j][2]-p[i][2]); 
        dist -= 0.2 * dotnorm(p[j], p[i]); // maximize negative cosine theta; orthogonal = 0.
        if(dist < min) min = dist; 
      }
    }
    if(min > bestd){
      bestd = min; 
      printf("----- maximizing min (dist - 0.2*cos(theta)) %f\n", bestd); 
      for(int j=0; j<NN; j++){
        printf("| %d -> (", layers[j]); 
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
