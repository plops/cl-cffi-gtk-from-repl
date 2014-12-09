
typedef struct obj obj;
struct obj {
  int set(obj*o,int val);
  int val;
};

int set(obj*o,int val)
{
  return o->val = val;
}

int init()
{
}
