#include <stdlib.h>
#include <stdio.h>
#include <time.h>

char * test0() {
    size_t i;
    char * start = malloc(2560);
    char * t = start;
        
    for (i=1; i<=2560; i++) {
        // delayedWord8
        char * p; 
        p = t;
        // undelay
        *p = (i%256);
        t++;
    }
    return start;
}

char * test1() {
    size_t i;
    char * start = malloc(2560*2);
    char * t = start;
       
    for (i=1; i<=2560; i++) {
        // delayedWord16
        unsigned short * p; 
        p = (unsigned short *)t;
        // undelay
        *p = i;
        t+=2;
    }
    return start;
}

main() {
    char * dat;
    struct timeval  tv1, tv2;
           /* stuff to do! */
    size_t i = 0;       
    dat = test0();
    free(dat);

    dat = test1();
    free(dat);
           
    double us = 0;
    double s = 0;
    for (i=0;i<10;i++) {
        gettimeofday(&tv1, NULL);
        dat = test0();
        gettimeofday(&tv2, NULL);
        free(dat);
        us += tv2.tv_usec - tv1.tv_usec;
        s  += tv2.tv_sec  - tv1.tv_sec;
    }
    printf ("Total time = %f seconds\n",
           (double) us/10/1000000 +
           (double) s/10);

    us = 0;
    s = 0;
    for (i=0;i<10;i++) {
        gettimeofday(&tv1, NULL);
        dat = test1();
        gettimeofday(&tv2, NULL);
        free(dat);
        us += tv2.tv_usec - tv1.tv_usec;
        s  += tv2.tv_sec  - tv1.tv_sec;
    }
    printf ("Total time = %f seconds\n",
           (double) us/10/1000000 +
           (double) s/10);

}
