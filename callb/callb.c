#include <malloc.h>
typedef struct T T;
struct T {
  int (*set)(T*o,int val);
  int (*callb)(T*o,void*data);
  int val;
};

static int set(T*o,int val)
{
  return o->val = val;
}

enum {N_TYPES=10};
T*types[N_TYPES];

int regist(int class_id,void*callb)
{
  if(class_id<N_TYPES){
    T* o = malloc(sizeof(T));
    o->set = set;
    o->callb = callb;
    types[class_id]=o;
    return 0;
  } else {
    return -1;
  }
}

int unregist(int class_id)
{
  if(class_id<0 || class_id>=N_TYPES || types[class_id]==NULL)
    return -1;
 
  free(types[class_id]);
  types[class_id]=NULL;
  return 0;
}
