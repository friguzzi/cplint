/*

EMBLEM and SLIPCASE

Copyright (c) 2013, Fabrizio Riguzzi and Elena Bellodi

This package uses the library cudd, see http://vlsi.colorado.edu/~fabio/CUDD/
for the relative license.

*/
#include <math.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include "cudd.h"
#include <SWI-Prolog.h>
#include <unistd.h>
#include <sys/types.h>

#ifdef _WIN32
#include <Windows.h>
#endif

#define BUFSIZE 200000
#define LOGZERO log(0.01)
#define CACHE_SLOTS 1
#define UNIQUE_SLOTS 1
#define RETURN_IF_FAIL if (ret!=TRUE) return ret;


typedef struct
{
  int nVal,nRule;
  int firstBoolVar;
  int abducible;
  int query;
} variable;


typedef struct
{
  DdNode *key;
  double value;
} rowel;

typedef struct
{
  int cnt;
  rowel *row;
} tablerow;


typedef struct
{
  DdManager * mgr; //Cudd manager
  int * bVar2mVar; //array that maps Boolena vars to multi-valued vars
  variable * vars; // multivalued variables
  int nVars;  // number of multivalued variables
  double * probs; // probabilities of Boolean variables
  int  boolVars;  // number of Boolean variables
  int nRules;  // number of rules
  int * rules; // array with the number of head atoms for each rule
  int n_abd;
  int n_abd_boolVars;
} environment;

typedef struct
{
  environment * env; // one evnironment for each example
  int ex;  // number of examples
  double * sigma; // sigma array for taking into account deleted paths
  double ***eta;  // eta array: for each rule, each Bool var stores two doubles
  double ***eta_temp; // eta arrau storing the contribution of the current example
  int * rules; // array with the number of head atoms for each rule
  int nRules; // number of rules
  double * nodes_probs;
  tablerow * nodesB; // tables of probabilities for nodes in Backward step
  tablerow * nodesFE; // tables of probabilities for nodes in Forward step
  tablerow * nodesFO; // tables of probabilities for nodes in Forward step
  double * example_prob; // probability (frequency) of examples in the data
} example_data;

typedef struct
{
  int var,val;
} assign;

typedef struct explan
{
  assign a;
  struct explan * next;
} explan_t;

typedef struct
{
  double prob;
  explan_t * mpa;
} prob_abd_expl;

typedef struct
{
  DdNode *node;
  int comp;
} explkey;

typedef struct
{
  explkey key;
  prob_abd_expl value;
} explrowel;

typedef struct
{
  int cnt;
  explrowel *row;
} expltablerow;




static foreign_t ret_prob(term_t,term_t,term_t);
static foreign_t ret_abd_prob(term_t,term_t,term_t,term_t);
static foreign_t ret_map_prob(term_t,term_t,term_t,term_t);
static foreign_t ret_vit_prob(term_t arg1, term_t arg2,
  term_t arg3, term_t arg4);
double Prob(DdNode *node,environment *env,tablerow *table);
prob_abd_expl abd_Prob(DdNode *node,environment *env,expltablerow *expltable,
  tablerow *table,
  int comp_par);
prob_abd_expl map_Prob(DdNode *node, environment * env,
    expltablerow * maptable, tablerow * table,
    int comp_par);
prob_abd_expl vit_Prob(DdNode *node, environment * env,
  expltablerow * expltable, tablerow * table,
  int comp_par);
static foreign_t end_bdd(term_t);
static foreign_t init_test(term_t, term_t);
static foreign_t add_var(term_t,term_t,term_t,term_t,term_t);
static foreign_t add_query_var(term_t,term_t,term_t,term_t,term_t);
static foreign_t add_abd_var(term_t,term_t,term_t,term_t,term_t);
static foreign_t init(term_t,term_t,term_t);
static foreign_t end(term_t);
static foreign_t EM(term_t,term_t,term_t,term_t,
  term_t,term_t,term_t,term_t);
static foreign_t reorder(term_t arg1);
static foreign_t make_query_var(term_t arg1, term_t arg2, term_t arg3);

double ProbPath(example_data * ex_d,DdNode *node, int nex);
//static int rec_deref(void);
void Forward(example_data * ex_d,DdNode *node, int nex);
void UpdateForward(example_data * ex_d,DdNode * node, int nex,
  DdNode *** nodesToVisit,int * NnodesToVisit);
double GetOutsideExpe(example_data *ex_d,DdNode *root,double ex_prob, int nex);
void Maximization(example_data * ex_d, double ** arrayprob);
static double Expectation(example_data *ex_d,DdNode **nodes_ex, int lenNodes);
int reorder_int(environment *env);

FILE *open_file(char *filename, const char *mode);
tablerow* init_table(int varcnt);
double * get_value(tablerow *tab,  DdNode *node);
void add_or_replace_node(tablerow *tab, DdNode *node, double value);
void add_node(tablerow *tab, DdNode *node, double value);
void destroy_table(tablerow *tab,int varcnt);
expltablerow* expl_init_table(int varcnt);
prob_abd_expl * expl_get_value(expltablerow *tab,  DdNode *node, int comp);
void expl_add_node(expltablerow *tab, DdNode *node, int comp, prob_abd_expl value);
void expl_destroy_table(expltablerow *tab,int varcnt);


install_t install(void);
void write_dot(environment * env, DdNode * bdd, FILE * file);

explan_t * insert(assign assignment,explan_t * head);
explan_t * duplicate(explan_t * head);
void free_list(explan_t * head);

term_t clist_to_pllist(explan_t *mpa, environment * env);
term_t abd_clist_to_pllist(explan_t *mpa);
term_t vit_clist_to_pllist(explan_t *mpa, environment * env);


static foreign_t init(term_t arg1,term_t arg2,term_t arg3)
{
  int j,i,ret;
  example_data * ex_d;
  double ***eta;
  double ***eta_temp;
  int nRules, *rules;
  term_t ex_d_t=PL_new_term_ref();
  term_t list=PL_copy_term_ref(arg2);
  term_t head=PL_new_term_ref();

  ex_d=(example_data *)malloc(sizeof(example_data));

  ex_d->ex=0;
  ret=PL_get_integer(arg1,&nRules);
  RETURN_IF_FAIL
  ex_d->nRules=nRules;
  ex_d->env=NULL;
  ex_d->eta= (double ***) malloc(nRules * sizeof(double **));
  eta=ex_d->eta;
  ex_d->eta_temp= (double ***) malloc(nRules * sizeof(double **));
  eta_temp=ex_d->eta_temp;
  ex_d->rules= (int *) malloc(nRules * sizeof(int));
  rules=ex_d->rules;
  ex_d->nodes_probs=NULL;
  for (j=0;j<nRules;j++)
  {
    ret=PL_get_list(list,head,list);
    RETURN_IF_FAIL
    ret=PL_get_integer(head,&rules[j]);
    RETURN_IF_FAIL
    eta[j]= (double **) malloc((rules[j]-1)*sizeof(double *));
    eta_temp[j]= (double **) malloc((rules[j]-1)*sizeof(double *));
    for (i=0;i<rules[j]-1;i++)
    {
      eta[j][i]=(double *) malloc(2*sizeof(double));
      eta_temp[j][i]=(double *) malloc(2*sizeof(double));
    }
  }
  ret=PL_put_pointer(ex_d_t,(void *)ex_d);
  RETURN_IF_FAIL
  return(PL_unify(ex_d_t,arg3));

}

static foreign_t init_bdd(term_t arg1, term_t arg2)
{
  example_data * ex_d;
  DdManager * mgr;
  term_t env_t;
  int ex,ret;

  env_t=PL_new_term_ref();
  ret=PL_get_pointer(arg1,(void **)&ex_d);
  RETURN_IF_FAIL
  ex=ex_d->ex;
  ex_d->env=(environment *) realloc(ex_d->env, (ex+1)*sizeof(environment));
  ex_d->env[ex].mgr=Cudd_Init(0,0,UNIQUE_SLOTS,CACHE_SLOTS,5120);
  mgr=ex_d->env[ex].mgr;
  Cudd_AutodynEnable(mgr, CUDD_REORDER_GROUP_SIFT);
  Cudd_SetMaxCacheHard(mgr, 0);
  Cudd_SetLooseUpTo(mgr, 0);
  Cudd_SetMinHit(mgr, 15);

  ex_d->env[ex].bVar2mVar=NULL;

  ex_d->env[ex].vars=NULL;

  ex_d->env[ex].nVars=0;

  ex_d->env[ex].probs=NULL;

  ex_d->env[ex].boolVars=0;

  ex_d->env[ex].nRules=ex_d->nRules;

  ex_d->env[ex].rules=ex_d->rules;

  ret=PL_put_pointer(env_t,(void *) (ex_d->env+ex));
  RETURN_IF_FAIL
  return(PL_unify(env_t,arg2));

}

static foreign_t end_bdd(term_t arg1)
{
  int ret;
  example_data *ex_d;

  ret=PL_get_pointer(arg1,(void **)&ex_d);
  RETURN_IF_FAIL
  ex_d->ex=ex_d->ex+1;
  PL_succeed;
}

static foreign_t init_test(term_t arg1,term_t arg2)
{
  term_t env_t;
  environment * env;
  int nRules,ret;
  ret=PL_get_integer(arg1,&nRules);
  RETURN_IF_FAIL
  env_t=PL_new_term_ref();

  env=(environment *)malloc(sizeof(environment));
  env->mgr=Cudd_Init(0,0,UNIQUE_SLOTS,CACHE_SLOTS,5120);
  env->n_abd=0;
  env->n_abd_boolVars=0;

  Cudd_AutodynEnable(env->mgr, CUDD_REORDER_GROUP_SIFT);
  Cudd_SetMaxCacheHard(env->mgr, 0);
  Cudd_SetLooseUpTo(env->mgr, 0);
  Cudd_SetMinHit(env->mgr, 15);

  env->bVar2mVar=NULL;
  env->vars=NULL;
  env->nVars=0;
  env->probs=NULL;
  env->boolVars=0;

  env->rules= (int *) malloc(nRules * sizeof(int));
  ret=PL_put_pointer(env_t,(void *) env);
  RETURN_IF_FAIL
  return(PL_unify(env_t,arg2));
}

static foreign_t end_test(term_t arg1)
{
  int ret;
  environment *env;

  ret=PL_get_pointer(arg1,(void **)&env);
  RETURN_IF_FAIL
  free(env->bVar2mVar);
  free(env->vars);
  Cudd_Quit(env->mgr);
  free(env->probs);
  free(env->rules);
  free(env);

  PL_succeed;
}



static double Expectation(example_data * ex_d,DdNode **nodes_ex,int lenNodes)
{
  int i;
  double rootProb,CLL=0;

  for(i=0;i<lenNodes;i++)
  {
    if (!Cudd_IsConstant(nodes_ex[i]))
    {
      ex_d->nodesB=init_table(ex_d->env[i].boolVars);
      ex_d->nodesFE=init_table(ex_d->env[i].boolVars);
      ex_d->nodesFO=init_table(ex_d->env[i].boolVars);

      Forward(ex_d,nodes_ex[i],i);
      rootProb=GetOutsideExpe(ex_d,nodes_ex[i],ex_d->example_prob[i],i);

      if (rootProb<=0.0)
        CLL = CLL + LOGZERO*ex_d->example_prob[i];
      else
        CLL = CLL + log(rootProb)*ex_d->example_prob[i];

      ex_d->nodes_probs[i]=rootProb;
      destroy_table(ex_d->nodesB,ex_d->env[i].boolVars);
      destroy_table(ex_d->nodesFE,ex_d->env[i].boolVars);
      destroy_table(ex_d->nodesFO,ex_d->env[i].boolVars);
    }
    else
      if (nodes_ex[i]==Cudd_ReadLogicZero(ex_d->env[i].mgr))
      {
        CLL=CLL+LOGZERO*ex_d->example_prob[i];
	ex_d->nodes_probs[i]=0.0;
      }
      else
        ex_d->nodes_probs[i]=1.0;
  }
  return CLL;
}

static foreign_t end(term_t arg1)
{
  int r,i,ret;
  example_data * ex_d;
  ret=PL_get_pointer(arg1,(void **)&ex_d);
  RETURN_IF_FAIL

  for (i=0;i<ex_d->ex;i++)
  {
    Cudd_Quit(ex_d->env[i].mgr);
    free(ex_d->env[i].bVar2mVar);
    free(ex_d->env[i].vars);
    free(ex_d->env[i].probs);
  }

  free(ex_d->env);
  for (r=0;r<ex_d->nRules;r++)
  {
    for (i=0;i<ex_d->rules[r]-1;i++)
    {
      free(ex_d->eta[r][i]);
      free(ex_d->eta_temp[r][i]);
    }
    free(ex_d->eta[r]);
    free(ex_d->eta_temp[r]);
  }
  free(ex_d->eta);
  free(ex_d->eta_temp);
  free(ex_d->rules);
  free(ex_d);
  PL_succeed;
}


static foreign_t ret_prob(term_t arg1, term_t arg2, term_t arg3)
{
  term_t out;
  environment * env;
  DdNode * node;
  tablerow * table;
  double prob;
  int ret;

  ret=PL_get_pointer(arg1,(void **)&env);
  RETURN_IF_FAIL
  ret=PL_get_pointer(arg2,(void **)&node);
  RETURN_IF_FAIL
  out=PL_new_term_ref();

  if (!Cudd_IsConstant(node))
  {
    table=init_table(env->boolVars);
    prob=Prob(node,env,table);
    if (Cudd_IsComplement(node))
      prob=1.0-prob;
    ret=PL_put_float(out,prob);
    RETURN_IF_FAIL
    destroy_table(table,env->boolVars);
  }
  else
  {
    if (node==Cudd_ReadOne(env->mgr))
    {
      ret=PL_put_float(out,1.0);
      RETURN_IF_FAIL
    }
    else
    {
      ret=PL_put_float(out,0.0);
      RETURN_IF_FAIL
    }
  }

  return(PL_unify(out,arg3));
}

int reorder_int(environment *env)
{
  int i,j,var_ind,abd_ind=0,ind=env->n_abd_boolVars;
  variable var,* vars=env->vars;
  DdManager *mgr=env->mgr;
  int boolVars=env->boolVars;
  int * permutation;
  int * bVar2mVar=env->bVar2mVar;

  permutation=malloc(boolVars*sizeof(int));
  for (i=0;i<boolVars;i++)
  {
    j=Cudd_ReadInvPerm(mgr,i);
    var_ind=bVar2mVar[j];
    var=vars[var_ind];
    if (var.abducible || var.query)
    {
      permutation[abd_ind]=j;
      abd_ind++;
    }
    else
    {
      permutation[ind]=j;
      ind++;
    }

  }
  return Cudd_ShuffleHeap(mgr,permutation);
}

static foreign_t reorder(term_t arg1)
{
  environment * env;
  int ret;

  ret=PL_get_pointer(arg1,(void **)&env);
  RETURN_IF_FAIL
  ret=reorder_int(env);
  RETURN_IF_FAIL
  return 1;
}

static foreign_t ret_abd_prob(term_t arg1, term_t arg2,
  term_t arg3, term_t arg4)
{
  term_t out,outass;
  environment * env;
  DdNode * node;
  expltablerow * expltable;
  tablerow * table;
  //abdtablerow * abdtable;
  prob_abd_expl delta;
  int ret;
  double p;
  explan_t * mpa;

  ret=PL_get_pointer(arg1,(void **)&env);
  RETURN_IF_FAIL
  ret=PL_get_pointer(arg2,(void **)&node);
  RETURN_IF_FAIL
  out=PL_new_term_ref();

  ret=reorder_int(env);
  RETURN_IF_FAIL

  if (!Cudd_IsConstant(node))
  {
    expltable=expl_init_table(env->boolVars);
    table=init_table(env->boolVars);
    //abdtable=init_abd_table(env->n_abd);

    delta=abd_Prob(node,env,expltable,table,0);
    p=delta.prob;
    mpa=delta.mpa;
    ret=PL_put_float(out,p);
    RETURN_IF_FAIL
    //destroy_table(abdtable,env->n_abd);
    outass=abd_clist_to_pllist(mpa);
    RETURN_IF_FAIL
    expl_destroy_table(expltable,env->boolVars);
    destroy_table(table,env->boolVars);
  }
  else
  {
    if (node==Cudd_ReadOne(env->mgr))
    {
      ret=PL_put_float(out,1.0);
      RETURN_IF_FAIL
    }
    else
    {
      ret=PL_put_float(out,0.0);
      RETURN_IF_FAIL
    }
      outass=PL_new_term_ref();
      ret=PL_put_nil(outass);
      RETURN_IF_FAIL
  }

  return(PL_unify(out,arg3)&&PL_unify(outass,arg4));
}

static foreign_t ret_map_prob(term_t arg1, term_t arg2,
  term_t arg3, term_t arg4)
{
  term_t out,outass;
  environment * env;
  DdNode * node;
  expltablerow * maptable;
  tablerow * table;
  //abdtablerow * abdtable;
  prob_abd_expl delta;
  int ret;
  double p;
  explan_t * mpa;

  ret=PL_get_pointer(arg1,(void **)&env);
  RETURN_IF_FAIL
  ret=PL_get_pointer(arg2,(void **)&node);
  RETURN_IF_FAIL
  out=PL_new_term_ref();

  ret=reorder_int(env);

  RETURN_IF_FAIL

  if (!Cudd_IsConstant(node))
  {
    maptable=expl_init_table(env->boolVars);
    table=init_table(env->boolVars);
    //abdtable=init_abd_table(env->n_abd);
    delta=map_Prob(node,env,maptable,table,0);
    p=delta.prob;
    mpa=delta.mpa;
    ret=PL_put_float(out,p);
    RETURN_IF_FAIL
    //destroy_table(abdtable,env->n_abd);
    outass=clist_to_pllist(mpa,env);
    RETURN_IF_FAIL
    expl_destroy_table(maptable,env->boolVars);
    destroy_table(table,env->boolVars);
  }
  else
  {
    if (node==Cudd_ReadOne(env->mgr))
    {
      ret=PL_put_float(out,1.0);
      RETURN_IF_FAIL
    }
    else
    {
      ret=PL_put_float(out,0.0);
      RETURN_IF_FAIL
    }
    outass=PL_new_term_ref();
    ret=PL_put_nil(outass);
    RETURN_IF_FAIL
  }

  return(PL_unify(out,arg3)&&PL_unify(outass,arg4));
}

static foreign_t ret_vit_prob(term_t arg1, term_t arg2,
  term_t arg3, term_t arg4)
{
  term_t out,outass;
  environment * env;
  DdNode * node;
  expltablerow * expltable;
  tablerow * table;
  //abdtablerow * abdtable;
  prob_abd_expl delta;
    //abdtable=init_abd_table(env->n_abd);
  int ret;
  double p;
  explan_t * mpa;

  ret=PL_get_pointer(arg1,(void **)&env);
  RETURN_IF_FAIL
  ret=PL_get_pointer(arg2,(void **)&node);
  RETURN_IF_FAIL
  out=PL_new_term_ref();


  if (!Cudd_IsConstant(node))
  {
    expltable=expl_init_table(env->boolVars);
    table=init_table(env->boolVars);
    //abdtable=init_abd_table(env->n_abd);
    delta=vit_Prob(node,env,expltable,table,0);
    p=delta.prob;
    mpa=delta.mpa;
    ret=PL_put_float(out,p);
    RETURN_IF_FAIL
    //destroy_table(abdtable,env->n_abd);
    outass=vit_clist_to_pllist(mpa,env);
    RETURN_IF_FAIL
    expl_destroy_table(expltable,env->boolVars);
    destroy_table(table,env->boolVars);
  }
  else
  {
    if (node==Cudd_ReadOne(env->mgr))
    {
      ret=PL_put_float(out,1.0);
      RETURN_IF_FAIL
    }
    else
    {
      ret=PL_put_float(out,0.0);
      RETURN_IF_FAIL
    }
      outass=PL_new_term_ref();
      ret=PL_put_nil(outass);
      RETURN_IF_FAIL
  }

  return(PL_unify(out,arg3)&&PL_unify(outass,arg4));
}

static foreign_t make_query_var(term_t arg1, term_t arg2, term_t arg3)
{
  environment * env;
  int ret,varIndex,i,j;
  DdNode * cons, * tmp1, * tmp2, * vari, * varj, * or, * tmpor;
  term_t out;
  variable var;

  ret=PL_get_pointer(arg1,(void **)&env);
  RETURN_IF_FAIL
  ret=PL_get_integer(arg2,&varIndex);
  RETURN_IF_FAIL


  //env->vars[varIndex].abducible_or_query=1;
  //env->n_abd++;
  var=env->vars[varIndex];

  cons=Cudd_ReadOne(env->mgr);
  or=Cudd_ReadLogicZero(env->mgr);

  for (i=var.firstBoolVar; i<var.firstBoolVar+var.nVal-1; i++)
  {
    vari=Cudd_bddIthVar(env->mgr,i);
    tmpor=Cudd_bddOr(env->mgr,or,vari);
    Cudd_Ref(tmpor);
    Cudd_RecursiveDeref(env->mgr,or);
    or=tmpor;
    for(j=i+1; j<var.firstBoolVar+var.nVal-1; j++)
    {
      varj=Cudd_bddIthVar(env->mgr,j);
      tmp1=Cudd_Not(Cudd_bddAnd(env->mgr,vari,varj));
      tmp2=Cudd_bddAnd(env->mgr,cons,tmp1);
      Cudd_Ref(tmp2);
      Cudd_RecursiveDeref(env->mgr,cons);
      cons=tmp2;
    }
  }
  tmpor=Cudd_bddOr(env->mgr,or,Cudd_bddIthVar(env->mgr,var.firstBoolVar+var.nVal-1));
  Cudd_Ref(tmpor);
  Cudd_RecursiveDeref(env->mgr,or);
  tmp1=Cudd_bddAnd(env->mgr,cons,tmpor);
  Cudd_Ref(tmp1);
  Cudd_RecursiveDeref(env->mgr,cons);
  cons=tmp1;

  out=PL_new_term_ref();
  ret=PL_put_pointer(out,(void *)cons);
  RETURN_IF_FAIL
  return(PL_unify(out,arg3));
}
term_t abd_clist_to_pllist(explan_t *mpa)
{
  term_t out,tail,head,var,val;
  functor_t minus;
  assign a;
  minus=PL_new_functor(PL_new_atom("-"), 2);
  out=PL_new_term_ref();
  head=PL_new_term_ref();
  int ret;
  if (mpa==NULL)
  {
    ret=PL_put_nil(out);
    RETURN_IF_FAIL
  }
  else
  {
    tail=abd_clist_to_pllist(mpa->next);
    a=mpa->a;
    var=PL_new_term_ref();
    val=PL_new_term_ref();
    ret=PL_put_integer(var,a.var);
    RETURN_IF_FAIL
    ret=PL_put_integer(val,a.val);
    RETURN_IF_FAIL
    ret=PL_cons_functor(head, minus,var,val);
    RETURN_IF_FAIL
    ret=PL_cons_list(out,head,tail);
    RETURN_IF_FAIL
  }
  return out;
}
term_t clist_to_pllist(explan_t *mpa, environment * env)
{
  term_t out,tail,head,var,val;
  functor_t minus;
  assign a;
  int value,bvar, ret, mvari, mval;
  variable mvar;

  minus=PL_new_functor(PL_new_atom("-"), 2);
  tail=PL_new_term_ref();
  ret=PL_put_nil(tail);
  RETURN_IF_FAIL
  if (mpa==NULL)
  {
    out=PL_new_term_ref();
    ret=PL_put_nil(out);
    RETURN_IF_FAIL
  }
  else
  {

    for (; mpa; mpa=mpa->next)
    {
      a=mpa->a;
      bvar=a.var;
      value=a.val;
      if (value)
      {
        mvari=env->bVar2mVar[bvar];
        mvar=env->vars[mvari];
        mval=a.var-mvar.firstBoolVar+1;
        var=PL_new_term_ref();
        val=PL_new_term_ref();
        ret=PL_put_integer(var,mvari);
        RETURN_IF_FAIL
        ret=PL_put_integer(val,mval);
        RETURN_IF_FAIL
        head=PL_new_term_ref();
        ret=PL_cons_functor(head, minus,var,val);
        RETURN_IF_FAIL
        out=PL_new_term_ref();
        ret=PL_cons_list(out,head,tail);
        RETURN_IF_FAIL
        tail=out;
      }
    }
    out=tail;
  }
  return out;
}

term_t vit_clist_to_pllist(explan_t *mpa, environment * env)
{
  term_t out,tail,head,var,val;
  functor_t minus;
  assign a;
  int value,bvar, ret, mvari, mval,nVars,i,*assignments;
  variable mvar;

  if (mpa==NULL)
  {
    out=PL_new_term_ref();
    ret=PL_put_nil(out);
    RETURN_IF_FAIL
  }
  else
  {
    nVars=env->nVars;
    assignments=malloc(nVars*sizeof(int));
    for (i=0;i<nVars;i++)
      assignments[i]=-1;

    for (; mpa; mpa=mpa->next)
    {
      a=mpa->a;
      bvar=a.var;
      value=a.val;
      mvari=env->bVar2mVar[bvar];
      mvar=env->vars[mvari];
      if (value)
      {
        mval=a.var-mvar.firstBoolVar;
        assignments[mvari]=mval;
      }
      else
      {
        assignments[mvari]=env->vars[mvari].nVal-1;
      }
    }
    minus=PL_new_functor(PL_new_atom("-"), 2);
    tail=PL_new_term_ref();
    ret=PL_put_nil(tail);
    RETURN_IF_FAIL
    for (i=0;i<nVars;i++)
    {
      if (assignments[i]!=-1)
      {
        var=PL_new_term_ref();
        val=PL_new_term_ref();
        ret=PL_put_integer(var,i);
        RETURN_IF_FAIL
        ret=PL_put_integer(val,assignments[i]);
        RETURN_IF_FAIL
        head=PL_new_term_ref();
        ret=PL_cons_functor(head, minus,var,val);
        RETURN_IF_FAIL
        out=PL_new_term_ref();
        ret=PL_cons_list(out,head,tail);
        RETURN_IF_FAIL
        tail=out;
      }
    }
    out=tail;
  }
  return out;
}

double Prob(DdNode *node, environment * env, tablerow * table)
/* compute the probability of the expression rooted at node.
table is used to store nodeB for which the probability has alread been computed
so that it is not recomputed
 */
{
  int index;
  double res;
  double p,pt,pf,BChild0,BChild1;
  double * value_p;
  DdNode *nodekey,*T,*F;
  //comp=(comp && !comp_par) ||(!comp && comp_par);
  if (Cudd_IsConstant(node))
  {
      return 1.0;
  }
  else
  {
    nodekey=Cudd_Regular(node);
    value_p=get_value(table,nodekey);
    if (value_p!=NULL)
        return *value_p;
    else
    {
      index=Cudd_NodeReadIndex(node);  //Returns the index of the node. The node pointer can be either regular or complemented.
      //The index field holds the name of the variable that labels the node. The index of a variable is a permanent attribute that reflects the order of creation.
      p=env->probs[index];
      T = Cudd_T(node);
      F = Cudd_E(node);
      pf=Prob(F,env,table);
      pt=Prob(T,env,table);
      if (Cudd_IsComplement(F))
        pf=1.0-pf;

      BChild0=pf*(1-p);
      BChild1=pt*p;
      res=BChild0+BChild1;
      add_node(table,nodekey,res);
      return res;
    }
  }
}

prob_abd_expl abd_Prob(DdNode *node, environment * env,
  expltablerow * expltable, tablerow * table,
  int comp_par)
/* compute the probability of the expression rooted at node.
table is used to store nodeB for which the probability has alread been computed
so that it is not recomputed
 */
{
  int index,comp,pos;
  double p,p0,p1;
  DdNode *nodekey,*T,*F;
  prob_abd_expl deltat,deltaf,delta,*deltaptr;
  assign assignment;
  explan_t * mpa0,* mpa1,* mpa;

  comp=Cudd_IsComplement(node);
  comp=(comp && !comp_par) ||(!comp && comp_par);
  index=Cudd_NodeReadIndex(node);
  pos=Cudd_ReadPerm(env->mgr,index);
  if (pos>=env->n_abd_boolVars)
  {
    p1=Prob(node,env,table);
    if (comp)
      p1= 1.0-p1;

    delta.prob=p1;
    delta.mpa=NULL;

    return delta;
  }
  else
  {
    nodekey=Cudd_Regular(node);
    deltaptr=expl_get_value(expltable,nodekey,comp);
    if (deltaptr!=NULL)
    {
      return *deltaptr;
    }
    else
    {

      T = Cudd_T(node);
      F = Cudd_E(node);
      deltaf=abd_Prob(F,env,expltable,table,comp);
      deltat=abd_Prob(T,env,expltable,table,comp);
      p=env->probs[index];

      if (p==1.0)
      {
        p0=deltaf.prob;
        p1=deltat.prob;
      }
      else
      {
        p0=deltaf.prob*(1-p);
        p1=deltat.prob*p;
      }

      mpa0=deltaf.mpa;
      mpa1=deltat.mpa;

      if (p1>p0)
      {
        assignment.var=env->bVar2mVar[index];
        assignment.val=1;
        mpa=insert(assignment,mpa1);
        delta.prob=p1;
        delta.mpa=mpa;
      }
      else
      {
        assignment.var=env->bVar2mVar[index];
        assignment.val=0;
        mpa=insert(assignment,mpa0);
        delta.prob=p0;
        delta.mpa=mpa;
      }
      expl_add_node(expltable,nodekey,comp,delta);
      return delta;
    }
  }
}

prob_abd_expl map_Prob(DdNode *node, environment * env,
  expltablerow * maptable, tablerow * table,
  int comp_par)
/* compute the probability of the expression rooted at node.
table is used to store nodeB for which the probability has alread been computed
so that it is not recomputed
 */
{
  int index,comp,pos;
  double p,p0,p1;
  DdNode *nodekey,*T,*F;
  prob_abd_expl deltat,deltaf,delta,*deltaptr;
  assign assignment;
  explan_t * mpa0,* mpa1,* mpa;


  index=Cudd_NodeReadIndex(node);
  pos=Cudd_ReadPerm(env->mgr,index);
  comp=Cudd_IsComplement(node);
  comp=(comp && !comp_par) ||(!comp && comp_par);

  if (pos>=env->n_abd_boolVars)
  {
    p1=Prob(node,env,table);
    if (comp)
      p1= 1.0-p1;
    delta.prob=p1;
    delta.mpa=NULL;


    return delta;
  }
  else
  {
    nodekey=Cudd_Regular(node);
    deltaptr=expl_get_value(maptable,nodekey,comp);
    if (deltaptr!=NULL)
    {
      return *deltaptr;
    }
    p=env->probs[index];
    T = Cudd_T(node);
    F = Cudd_E(node);
    deltaf=map_Prob(F,env,maptable,table,comp);
    deltat=map_Prob(T,env,maptable,table,comp);

    p0=deltaf.prob;
    mpa0=deltaf.mpa;

    p1=deltat.prob*p;
    mpa1=deltat.mpa;

    if (p1>p0)
    {
      assignment.var=index;
      assignment.val=1;
      mpa=insert(assignment,mpa1);
      delta.prob=p1;
      delta.mpa=mpa;
    }
    else
    {
      assignment.var=index;
      assignment.val=0;
      mpa=insert(assignment,mpa0);
      delta.prob=p0;
      delta.mpa=mpa;
    }
    expl_add_node(maptable,nodekey,comp,delta);
    return delta;
  }

}
prob_abd_expl vit_Prob(DdNode *node, environment * env,
  expltablerow * expltable, tablerow * table,
  int comp_par)
/* compute the probability of the expression rooted at node.
table is used to store nodeB for which the probability has alread been computed
so that it is not recomputed
 */
{
  int index,comp;
  double p,p0,p1;
  DdNode *nodekey,*T,*F;
  prob_abd_expl deltat,deltaf,delta,*deltaptr;
  assign assignment;
  explan_t * mpa0,* mpa1,* mpa;

  comp=Cudd_IsComplement(node);
  comp=(comp && !comp_par) ||(!comp && comp_par);
  index=Cudd_NodeReadIndex(node);
  if (Cudd_IsConstant(node))
  {

    if (comp)
      p1= 0.0;
    else
      p1= 1.0;

    delta.prob=p1;
    delta.mpa=NULL;

    return delta;
  }
  else
  {
    nodekey=Cudd_Regular(node);
    deltaptr=expl_get_value(expltable,nodekey,comp);
    if (deltaptr!=NULL)
    {
      return *deltaptr;
    }
    else
    {

      T = Cudd_T(node);
      F = Cudd_E(node);
      deltaf=vit_Prob(F,env,expltable,table,comp);
      deltat=vit_Prob(T,env,expltable,table,comp);
      p=env->probs[index];


      p0=deltaf.prob*(1-p);
      p1=deltat.prob*p;

      mpa0=deltaf.mpa;
      mpa1=deltat.mpa;

      if (p1>p0)
      {
        assignment.var=index;
        assignment.val=1;
        mpa=insert(assignment,mpa1);
        delta.prob=p1;
        delta.mpa=mpa;
      }
      else
      {
        assignment.var=index;
        assignment.val=0;
        mpa=insert(assignment,mpa0);
        delta.prob=p0;
        delta.mpa=mpa;
      }
      expl_add_node(expltable,nodekey,comp,delta);
      return delta;
    }
  }
}

explan_t * insert(assign assignment,explan_t * head)
{
  explan_t * newhead;

  newhead=malloc(sizeof(explan_t));
  newhead->a=assignment;
  newhead->next=duplicate(head);
  return newhead;
}

explan_t * duplicate(explan_t * head)
{
  explan_t * newhead;
  if (head)
  {
    newhead=malloc(sizeof(explan_t));
    newhead->a=head->a;
    newhead->next=duplicate(head->next);
  }
  else
    newhead=NULL;
  return newhead;
}

void free_list(explan_t * head)
{
  if (head)
  {
    free_list(head->next);
    free(head);
  }
}
static foreign_t add_var(term_t arg1,term_t arg2,term_t arg3,term_t arg4, term_t arg5)
{
  term_t out,head,probTerm;
  variable * v;
  int i,ret;
  double p,p0;
  environment * env;

  head=PL_new_term_ref();
  out=PL_new_term_ref();
  ret=PL_get_pointer(arg1,(void **)&env);
  RETURN_IF_FAIL
  env->nVars=env->nVars+1;
  env->vars=(variable *) realloc(env->vars,env->nVars * sizeof(variable));

  v=&env->vars[env->nVars-1];
  v->abducible=0;
  v->query=0;
  ret=PL_get_integer(arg2,&v->nVal);
  RETURN_IF_FAIL
  ret=PL_get_integer(arg4,&v->nRule);
  RETURN_IF_FAIL

  v->firstBoolVar=env->boolVars;
  env->probs=(double *) realloc(env->probs,(((env->boolVars+v->nVal-1)* sizeof(double))));
  env->bVar2mVar=(int *) realloc(env->bVar2mVar,((env->boolVars+v->nVal-1)* sizeof(int)));
  probTerm=PL_copy_term_ref(arg3);
  p0=1;
  for (i=0;i<v->nVal-1;i++)
  {
    ret=PL_get_list(probTerm,head,probTerm);
    RETURN_IF_FAIL
    ret=PL_get_float(head,&p);
    RETURN_IF_FAIL
    env->bVar2mVar[env->boolVars+i]=env->nVars-1;
    env->probs[env->boolVars+i]=p/p0;
    p0=p0*(1-p/p0);
  }
  env->boolVars=env->boolVars+v->nVal-1;
  env->rules[v->nRule]= v->nVal;

  ret=PL_put_integer(out,env->nVars-1);
  RETURN_IF_FAIL

  return(PL_unify(out,arg5));
}

static foreign_t add_query_var(term_t arg1,term_t arg2,term_t arg3,term_t arg4, term_t arg5)
{
  term_t out,head,probTerm;
  variable * v;
  int i,ret;
  double p;
  environment * env;

  head=PL_new_term_ref();
  out=PL_new_term_ref();
  ret=PL_get_pointer(arg1,(void **)&env);
  RETURN_IF_FAIL
  env->nVars=env->nVars+1;
  env->n_abd++;
  env->vars=(variable *) realloc(env->vars,env->nVars * sizeof(variable));

  v=&env->vars[env->nVars-1];
  v->query=1;
  v->abducible=0;
  ret=PL_get_integer(arg2,&v->nVal);
  RETURN_IF_FAIL
  ret=PL_get_integer(arg4,&v->nRule);
  RETURN_IF_FAIL

  env->n_abd_boolVars=env->n_abd_boolVars+v->nVal;
  v->firstBoolVar=env->boolVars;
  env->probs=(double *) realloc(env->probs,(((env->boolVars+v->nVal)* sizeof(double))));
  env->bVar2mVar=(int *) realloc(env->bVar2mVar,((env->boolVars+v->nVal)* sizeof(int)));
  probTerm=PL_copy_term_ref(arg3);

  for (i=0;i<v->nVal;i++)
  {
    ret=PL_get_list(probTerm,head,probTerm);
    RETURN_IF_FAIL
    ret=PL_get_float(head,&p);
    RETURN_IF_FAIL
    env->bVar2mVar[env->boolVars+i]=env->nVars-1;
    env->probs[env->boolVars+i]=p;
  }
  env->boolVars=env->boolVars+v->nVal;
  env->rules[v->nRule]= v->nVal;

  ret=PL_put_integer(out,env->nVars-1);
  RETURN_IF_FAIL

  return(PL_unify(out,arg5));
}

static foreign_t add_abd_var(term_t arg1,term_t arg2,term_t arg3,term_t arg4, term_t arg5)
{
  term_t out,head,probTerm;
  variable * v;
  int i,ret;
  double p,p0;
  environment * env;

  head=PL_new_term_ref();
  out=PL_new_term_ref();
  ret=PL_get_pointer(arg1,(void **)&env);
  RETURN_IF_FAIL
  env->nVars=env->nVars+1;
  env->vars=(variable *) realloc(env->vars,env->nVars * sizeof(variable));

  env->n_abd=env->n_abd+1;

  v=&env->vars[env->nVars-1];

  v->abducible=1;
  v->query=0;
  ret=PL_get_integer(arg2,&v->nVal);
  RETURN_IF_FAIL
  env->n_abd_boolVars=env->n_abd_boolVars+v->nVal-1;

  ret=PL_get_integer(arg4,&v->nRule);
  RETURN_IF_FAIL
  v->firstBoolVar=env->boolVars;
  env->probs=(double *) realloc(env->probs,(((env->boolVars+v->nVal-1)* sizeof(double))));
  env->bVar2mVar=(int *) realloc(env->bVar2mVar,((env->boolVars+v->nVal-1)* sizeof(int)));
  probTerm=PL_copy_term_ref(arg3);
  p0=1;
  for (i=0;i<v->nVal-1;i++)
  {
    ret=PL_get_list(probTerm,head,probTerm);
    RETURN_IF_FAIL
    ret=PL_get_float(head,&p);
    RETURN_IF_FAIL
    env->bVar2mVar[env->boolVars+i]=env->nVars-1;
    env->probs[env->boolVars+i]=p/p0;
    p0=p0*(1-p/p0);
  }
  env->boolVars=env->boolVars+v->nVal-1;
  env->rules[v->nRule]= v->nVal;
  ret=PL_put_integer(out,env->nVars-1);
  RETURN_IF_FAIL

  return(PL_unify(out,arg5));
}

static foreign_t equality(term_t arg1,term_t arg2,term_t arg3, term_t arg4)
{
  term_t out;
  int varIndex;
  int value;
  int i,ret;
  variable v;
  DdNode * node, * tmp,*var;
  environment * env;

  ret=PL_get_pointer(arg1,(void **)&env);
  RETURN_IF_FAIL

  ret=PL_get_integer(arg2,&varIndex);
  RETURN_IF_FAIL
  ret=PL_get_integer(arg3,&value);
  RETURN_IF_FAIL
  v=env->vars[varIndex];
  i=v.firstBoolVar;
  tmp=Cudd_ReadOne(env->mgr);
  Cudd_Ref(tmp);
  node=NULL;
  if (v.query)
  {
    var=Cudd_bddIthVar(env->mgr,v.firstBoolVar+value);
    node=Cudd_bddAnd(env->mgr,tmp,var);
    Cudd_Ref(node);
  }
  else
  {
    for (i=v.firstBoolVar;i<v.firstBoolVar+value;i++)
    {
      var=Cudd_bddIthVar(env->mgr,i);
      node=Cudd_bddAnd(env->mgr,tmp,Cudd_Not(var));
      Cudd_Ref(node);
      Cudd_RecursiveDeref(env->mgr,tmp);
      tmp=node;
    }
    if (!(value==v.nVal-1))
    {
      var=Cudd_bddIthVar(env->mgr,v.firstBoolVar+value);
      node=Cudd_bddAnd(env->mgr,tmp,var);
      Cudd_Ref(node);
      Cudd_RecursiveDeref(env->mgr,tmp);
    }
  }
  out=PL_new_term_ref();
  ret=PL_put_pointer(out,(void *)node);
  RETURN_IF_FAIL
  return(PL_unify(out,arg4));
}

static foreign_t one(term_t arg1, term_t arg2)
{
  term_t out;
  DdNode * node;
  environment *env;
  int res,ret;

  ret=PL_get_pointer(arg1,(void **)&env);
  RETURN_IF_FAIL

  node =  Cudd_ReadOne(env->mgr);
  Cudd_Ref(node);
  out=PL_new_term_ref();
  ret=PL_put_pointer(out,(void *) node);
  RETURN_IF_FAIL
  res=PL_unify(out,arg2);
  return res;

//  return(PL_unify(out,arg2));
}

static foreign_t zero(term_t arg1, term_t arg2)
{
  term_t out;
  DdNode * node;
  environment *env;
  int ret;

  ret=PL_get_pointer(arg1,(void **)&env);
  RETURN_IF_FAIL

  node = Cudd_ReadLogicZero(env->mgr);
  Cudd_Ref(node);
  out=PL_new_term_ref();
  ret=PL_put_pointer(out,(void *) node);
  RETURN_IF_FAIL
  return(PL_unify(out,arg2));
}

static foreign_t bdd_not(term_t arg1,term_t arg2, term_t arg3)
{
  term_t out;
  DdNode * node;
  int ret;

  ret=PL_get_pointer(arg2,(void **)&node);
  RETURN_IF_FAIL
  node=Cudd_Not(node);
  out=PL_new_term_ref();
  ret=PL_put_pointer(out,(void *) node);
  RETURN_IF_FAIL
  return(PL_unify(out,arg3));
}

static foreign_t and(term_t arg1,term_t arg2,term_t arg3, term_t arg4)
{
  term_t out;
  DdNode * node1, *node2,*nodeout;
  environment *env;
  int res,ret;
  ret=PL_get_pointer(arg1,(void **)&env);
  RETURN_IF_FAIL

  ret=PL_get_pointer(arg2,(void **)&node1);
  RETURN_IF_FAIL
  ret=PL_get_pointer(arg3,(void **)&node2);
  RETURN_IF_FAIL
  nodeout=Cudd_bddAnd(env->mgr,node1,node2);
  Cudd_Ref(nodeout);
  out=PL_new_term_ref();
  ret=PL_put_pointer(out,(void *) nodeout);
  RETURN_IF_FAIL
  res=PL_unify(out,arg4);
  return res;
}

static foreign_t or(term_t arg1,term_t arg2,term_t arg3, term_t arg4)
{
  term_t out;
  DdNode * node1, *node2,*nodeout;
  environment *env;
  int ret;

  ret=PL_get_pointer(arg1,(void **)&env);
  RETURN_IF_FAIL

  ret=PL_get_pointer(arg2,(void **)&node1);
  RETURN_IF_FAIL
  ret=PL_get_pointer(arg3,(void **)&node2);
  RETURN_IF_FAIL

  nodeout=Cudd_bddOr(env->mgr,node1,node2);
  Cudd_Ref(nodeout);
  out=PL_new_term_ref();
  ret=PL_put_pointer(out,(void *) nodeout);
  RETURN_IF_FAIL
  return(PL_unify(out,arg4));
}

/*
static int garbage_collect(void)
{
  YAP_Term arg1,arg2,out;
  YAP_Int nodes,clearCache;

  arg1=YAP_ARG1;
  arg2=YAP_ARG2;
  clearCache=YAP_IntOfTerm(arg1);
  nodes=(YAP_Int)cuddGarbageCollect(mgr_ex[ex],clearCache);
  out=YAP_MkIntTerm(nodes);
  return(YAP_Unify(out,arg2));
}

static int bdd_to_add(void)
{
  YAP_Term arg1,arg2,out;
  DdNode * node1,*node2;

  arg1=YAP_ARG1;
  arg2=YAP_ARG2;
  node1=(DdNode *)YAP_IntOfTerm(arg1);
  node2= Cudd_BddToAdd(mgr_ex[ex],node1);
  out=YAP_MkIntTerm((YAP_Int) node2);
  return(YAP_Unify(out,arg2));
}
*/
static foreign_t create_dot(term_t arg1, term_t arg2, term_t arg3)
{
  DdNode * node;
  environment *env;
  char *filename;
  FILE * file;
  int ret;
  ret=PL_get_pointer(arg1,(void **)&env);
  RETURN_IF_FAIL
  ret=PL_get_pointer(arg2,(void **)&node);
  RETURN_IF_FAIL
  ret=PL_get_file_name(arg3,&filename,0);
  RETURN_IF_FAIL
  file = open_file(filename, "w");
  write_dot(env,node,file);
  fclose(file);
  return TRUE;
}

static foreign_t create_dot_string(term_t arg1, term_t arg2, term_t arg3)
{
  term_t out;
  DdNode * node;
  environment *env;
  FILE * file;
  char *buffer=NULL;
  int ret;

  ret=PL_get_pointer(arg1,(void **)&env);
  RETURN_IF_FAIL
  ret=PL_get_pointer(arg2,(void **)&node);
  RETURN_IF_FAIL
  out=PL_new_term_ref();

#ifndef _WIN32
  file=tmpfile();
#else
  char filename[MAX_PATH];
  GetTempFileName(".","temp",0,filename);
  file = fopen(filename,"w+bTD");
#endif
  if (file==NULL) {perror("Error in temporary file opening");}
  write_dot(env,node,file);

  if (fseek(file, 0L, SEEK_END) == 0) {
    /* Get the size of the file. */
    long bufsize = ftell(file);
    if (bufsize == -1) { perror("Error in getting the size of the temporary file");}

      /* Allocate our buffer to that size. */
        buffer = malloc(sizeof(char) * (bufsize + 1));

        /* Go back to the start of the file. */
        if (fseek(file, 0L, SEEK_SET) != 0) { perror("Error going back to the start of the file");}

        /* Read the entire file into memory. */
        size_t newLen = fread(buffer, sizeof(char), bufsize, file);
        if ( ferror( file ) != 0 ) {
            perror("Error reading file");
        } else {
            buffer[newLen++] = '\0'; /* Just to be safe. */
        }
  }
  fclose(file);
  ret=PL_put_string_chars(out,buffer);
  RETURN_IF_FAIL
  return(PL_unify(out,arg3));
}

void write_dot(environment * env, DdNode * bdd, FILE * file)
{
  char * onames[]={"Out"};
  char ** inames;
  int i,b,index,nv;
  variable v;
  char numberVar[11],numberBit[11];
  inames= (char **) malloc(sizeof(char *)*(env->boolVars));
  index=0;
  for (i=0;i<env->nVars;i++)
  {
    v=env->vars[i];
      if (v.query)
        nv=v.nVal;
      else
        nv=v.nVal-1;
    for (b=0;b<nv;b++)
    {
      inames[b+index]=(char *) malloc(sizeof(char)*20);
      strcpy(inames[b+index],"X");
      sprintf(numberVar,"%d",i);
      strcat(inames[b+index],numberVar);
      strcat(inames[b+index],"_");
      sprintf(numberBit,"%d",b);
      strcat(inames[b+index],numberBit);
    }
    index=index+nv;
  }
  Cudd_DumpDot(env->mgr,1,&bdd,(const char * const *)inames,(const char * const *)onames,file);
  index=0;
  for (i=0;i<env->nVars;i++)
  {
    v=env->vars[i];
    if (v.query)
      nv=v.nVal;
    else
      nv=v.nVal-1;
    for (b=0;b<nv;b++)
    {
      free(inames[b+index]);
    }
    index=index+nv;
  }
  free(inames);
}

/*
static int rec_deref(void)
{
  YAP_Term arg1;
  DdNode * node;

  arg1=YAP_ARG1;
  node=(DdNode *) YAP_IntOfTerm(arg1);
  Cudd_RecursiveDeref(mgr_ex[ex], node);
  return 1;
}

*/

double ProbPath(example_data * ex_d,DdNode *node, int nex)
{
  int index,mVarIndex,pos,position;//,boolVarIndex;
  variable v;
  double res;
  double p,pt,pf,BChild0e,BChild1e,BChild0o,BChild1o,e0,e1;
  double *value_p, * value_p_e, *value_p_o,** eta_rule;
  DdNode *nodekey,*T,*F;

  // printf("node %p comp %d\n",node,comp );
  if (Cudd_IsConstant(node))
  {
    // printf("constant\n");
    return 1.0;
  }
  else
  {
    nodekey=Cudd_Regular(node);
    value_p=get_value(ex_d->nodesB,nodekey);
    if (value_p!=NULL)
    {
      // printf("found %f\n", *value_p);
      return *value_p;
    }
    else
    {
      index=Cudd_NodeReadIndex(node);
      p=ex_d->env[nex].probs[index];
      T = Cudd_T(node);
      F = Cudd_E(node);
      pf=ProbPath(ex_d,F,nex);
      pt=ProbPath(ex_d,T,nex);
      // printf("pt %f pf %f\n",pt,pf );
      if (Cudd_IsComplement(F))
        pf=1.0-pf;
      // printf("pt %f pf %f\n",pt,pf );

      BChild0e=pf*(1-p);
      BChild0o=(1-pf)*(1-p);
      BChild1e=pt*p;
      BChild1o=(1-pt)*p;
      value_p_e=get_value(ex_d->nodesFE,nodekey);
      value_p_o=get_value(ex_d->nodesFO,nodekey);
      e0 = (*value_p_e)*BChild0e+(*value_p_o)*BChild0o;
      e1 = (*value_p_e)*BChild1e+(*value_p_o)*BChild1o;
    //  printf("e node %p %f %f %f %f\n",node,*value_p_e,*value_p_o,e0,e1 );
      mVarIndex=ex_d->env[nex].bVar2mVar[index];
      v=ex_d->env[nex].vars[mVarIndex];
      pos=index-v.firstBoolVar;
      eta_rule=ex_d->eta_temp[v.nRule];
      eta_rule[pos][0]=eta_rule[pos][0]+e0;
      eta_rule[pos][1]=eta_rule[pos][1]+e1;
      res=BChild0e+BChild1e;
      add_node(ex_d->nodesB,nodekey,res);
      position=Cudd_ReadPerm(ex_d->env[nex].mgr,index);
      position=position+1;
//      boolVarIndex=Cudd_ReadInvPerm(ex_d->env[nex].mgr,position);//Returns the index of the variable currently in the i-th position of the order.
      if (position<ex_d->env[nex].boolVars)
      {
        ex_d->sigma[position]=ex_d->sigma[position]+e0+e1;
      }
      if(!Cudd_IsConstant(T))
      {
        index=Cudd_NodeReadIndex(T);
        position=Cudd_ReadPerm(ex_d->env[nex].mgr,index);
        ex_d->sigma[position]=ex_d->sigma[position]-e1;
      }

      if(!Cudd_IsConstant(F))
      {
        index=Cudd_NodeReadIndex(F);
        position=Cudd_ReadPerm(ex_d->env[nex].mgr,index);
        ex_d->sigma[position]=ex_d->sigma[position]-e0;
      }

      return res;
    }
  }
}




void Forward(example_data * ex_d,DdNode *root, int nex)
{
  DdNode *** nodesToVisit;
  int * NnodesToVisit,comp;
  double FrootE,FrootO;

  environment env;
  int i,j;
  env=ex_d->env[nex];
  comp=Cudd_IsComplement(root);
  if (comp)
  {
    FrootE=0.0;
    FrootO=1.0;
  }
  else
  {
    FrootE=1.0;
    FrootO=0.0;
  }
  add_node(ex_d->nodesFE,Cudd_Regular(root),FrootE);
  add_node(ex_d->nodesFO,Cudd_Regular(root),FrootO);
  if (env.boolVars)
  {
    nodesToVisit= (DdNode ***)malloc(sizeof(DdNode **)* env.boolVars);
    NnodesToVisit= (int *)malloc(sizeof(int)* env.boolVars);
    nodesToVisit[0]=(DdNode **)malloc(sizeof(DdNode *));
    nodesToVisit[0][0]=root;
    NnodesToVisit[0]=1;
    for(i=1;i<env.boolVars;i++)
    {
      nodesToVisit[i]=NULL;
      NnodesToVisit[i]=0;
    }
    for(i=0;i<env.boolVars;i++)
    {
      for(j=0;j<NnodesToVisit[i];j++)
      UpdateForward(ex_d,nodesToVisit[i][j],nex,nodesToVisit,NnodesToVisit);
    }
    for(i=0;i<env.boolVars;i++)
    {
      free(nodesToVisit[i]);
    }
    free(nodesToVisit);
    free(NnodesToVisit);
  }
}

void UpdateForward(example_data *ex_d,DdNode *node, int nex,
  DdNode *** nodesToVisit, int * NnodesToVisit)
{
  int index,position;
  DdNode *T,*E,*nodereg;
  double *value_p_E,*value_p_O,*value_p_T_E,*value_p_T_O,
    *value_p_F_E,*value_p_F_O,p,value_par_E,value_par_O;

// printf("F node %p comp %d\n",node,Cudd_IsComplement(node) );
  if (Cudd_IsConstant(node))
  {
    return;
  }
  else
  {
    index=Cudd_NodeReadIndex(node);
    p=ex_d->env[nex].probs[index];
    nodereg=Cudd_Regular(node);
    value_p_E=get_value(ex_d->nodesFE,nodereg);
    value_p_O=get_value(ex_d->nodesFO,nodereg);
    if (value_p_E== NULL)
    {
      printf("Error\n");
      return;
    }
    else
    {
      T = Cudd_T(node);
      E = Cudd_E(node);
      if (!Cudd_IsConstant(T))
      {
        value_p_T_E=get_value(ex_d->nodesFE,T);
        value_p_T_O=get_value(ex_d->nodesFO,T);
        if (value_p_T_E!= NULL)
        {
           *value_p_T_E= *value_p_T_E+*value_p_E*p;
           *value_p_T_O= *value_p_T_O+*value_p_O*p;
          // printf("update f t %p %f %f %f %f\n",T,*value_p_T_E,
          //  *value_p_E*p,*value_p_T_O,*value_p_O*p);
        }
        else
        {
          // printf("new f t %p %f %f \n",T,*value_p_E*p,*value_p_O*p );

          add_or_replace_node(ex_d->nodesFE,Cudd_Regular(T),*value_p_E*p);
          add_or_replace_node(ex_d->nodesFO,Cudd_Regular(T),*value_p_O*p);
          index=Cudd_NodeReadIndex(T);
          position=Cudd_ReadPerm(ex_d->env[nex].mgr,index);
          nodesToVisit[position]=(DdNode **)realloc(nodesToVisit[position],
	    (NnodesToVisit[position]+1)* sizeof(DdNode *));
          nodesToVisit[position][NnodesToVisit[position]]=T;
          NnodesToVisit[position]=NnodesToVisit[position]+1;
        }
      }
      if (!Cudd_IsConstant(E))
      {
        value_p_F_E=get_value(ex_d->nodesFE,Cudd_Regular(E));
        value_p_F_O=get_value(ex_d->nodesFO,Cudd_Regular(E));
        // if (Cudd_IsComplement(E))
        //   value_par=1 - *value_p;
        // else
        //   value_par= *value_p;
        // if (Cudd_IsComplement(E))
        //   p=1 -p;
        if (Cudd_IsComplement(E))
        {
          value_par_E= *value_p_O;
          value_par_O= *value_p_E;
        }
        else
        {
          value_par_E= *value_p_E;
          value_par_O= *value_p_O;
        }
        // printf("f child %d %f %f\n",Cudd_IsComplement(E),value_par_E,value_par_O );

        if (value_p_F_E!= NULL)
        {

          *value_p_F_E= *value_p_F_E+value_par_E*(1-p);
          *value_p_F_O= *value_p_F_O+value_par_O*(1-p);
          // printf("update f f %p %f %f %f %f\n",E,*value_p_F_E, value_par_E*(1-p),
        // *value_p_F_O, value_par_O*(1-p));
        }
        else
        {
                              // printf("new f f %p %f\n",E,value_par_E*(1-p) );

          add_or_replace_node(ex_d->nodesFE,Cudd_Regular(E),value_par_E*(1-p));
          add_or_replace_node(ex_d->nodesFO,Cudd_Regular(E),value_par_O*(1-p));
          index=Cudd_NodeReadIndex(E);
          position=Cudd_ReadPerm(ex_d->env[nex].mgr,index);
          nodesToVisit[position]=(DdNode **)realloc(nodesToVisit[position],
	    (NnodesToVisit[position]+1)* sizeof(DdNode *));
          nodesToVisit[position][NnodesToVisit[position]]=E;
          NnodesToVisit[position]=NnodesToVisit[position]+1;
        }
      }
      return;
    }
  }
}




double GetOutsideExpe(example_data * ex_d,DdNode *root,double ex_prob, int nex)
{
  int i,j,mVarIndex,bVarIndex,firstBoolVarOfRule;
  double **eta_rule;
  double theta,rootProb, T=0;


  ex_d->sigma=(double *)malloc(ex_d->env[nex].boolVars * sizeof(double));

  for (j=0; j<ex_d->env[nex].boolVars; j++)
  {
    ex_d->sigma[j]=0;
  }
  for (j=0; j<ex_d->nRules; j++)
  {
    for (i=0; i<ex_d->rules[j]-1; i++)
    {
      ex_d->eta_temp[j][i][0]=0;
      ex_d->eta_temp[j][i][1]=0;
    }
  }
  rootProb=ProbPath(ex_d,root,nex);
  if (Cudd_IsComplement(root))
    rootProb=1.0-rootProb;
  if (rootProb>0.0)
  {
    for (j=0; j<ex_d->env[nex].boolVars; j++)
    {
      T += ex_d->sigma[j];
      bVarIndex=Cudd_ReadInvPerm(ex_d->env[nex].mgr,j);
      if (bVarIndex==-1)
      {
        bVarIndex=j;
      }

      mVarIndex=ex_d->env[nex].bVar2mVar[bVarIndex];
      eta_rule=ex_d->eta_temp[ex_d->env[nex].vars[mVarIndex].nRule];
      firstBoolVarOfRule=ex_d->env[nex].vars[mVarIndex].firstBoolVar;
      i=bVarIndex-firstBoolVarOfRule;
      theta=ex_d->env[nex].probs[bVarIndex];
      eta_rule[i][0]=eta_rule[i][0]+T*(1-theta);
      eta_rule[i][1]=eta_rule[i][1]+T*theta;
    }

    for (j=0; j<ex_d->nRules; j++)
    {
      for (i=0; i<ex_d->rules[j]-1; i++)
      {
        ex_d->eta[j][i][0]=ex_d->eta[j][i][0]+
	  ex_d->eta_temp[j][i][0]*ex_prob/rootProb;
        ex_d->eta[j][i][1]=ex_d->eta[j][i][1]+
	  ex_d->eta_temp[j][i][1]*ex_prob/rootProb;
      }
    }
  }
  free(ex_d->sigma);
  return rootProb;
}


void Maximization(example_data * ex_d, double ** arrayprob)
{
  int r,i,j,e;
  double sum=0;
  double *probs_rule,**eta_rule;

  for (r=0;r<ex_d->nRules;r++)
  {
    eta_rule=ex_d->eta[r];
    for (i=0;i<ex_d->rules[r]-1;i++)
    {
      sum=(eta_rule[i][0]+eta_rule[i][1]);
      if (sum==0.0)
      {
        arrayprob[r][i]=0;
      }
      else
        arrayprob[r][i]=eta_rule[i][1]/sum;
    }
  }

  for(e=0;e<ex_d->ex;e++)
  {
    for (j=0;j<ex_d->env[e].nVars;j++)
    {
      r=ex_d->env[e].vars[j].nRule;
      probs_rule=arrayprob[r];
      for(i=0;i<ex_d->rules[r]-1;i++)
      {
        ex_d->env[e].probs[ex_d->env[e].vars[j].firstBoolVar+i]=probs_rule[i];
      }
    }
  }
}

static foreign_t randomize(term_t arg1)
{
  int i,j,e,rule,ret;
  double * theta,p0;
  double pmass,par;
  double **Theta_rules;
  example_data * ex_d;

  ret=PL_get_pointer(arg1,(void **)&ex_d);
  RETURN_IF_FAIL


  Theta_rules=(double **)malloc(ex_d->nRules *sizeof(double *));

  for (j=0;j<ex_d->nRules;j++)
  {
    Theta_rules[j]=(double *)malloc(ex_d->rules[j]*sizeof(double));
  }

  for (j=0;j<ex_d->nRules;j++)
  {
    theta=Theta_rules[j];
    pmass=0;
    for (i=0;i<ex_d->rules[j]-1;i++)
    {
      par=((double)rand())/RAND_MAX*(1-pmass);
      pmass=pmass+par;
      theta[i]=par;
    }
    theta[ex_d->rules[j]-1]=1-pmass;
  }
  for(e=0;e<ex_d->ex;e++)
  {
    for (j=0; j<ex_d->env[e].nVars; j++)
    {
      rule=ex_d->env[e].vars[j].nRule;
      theta=Theta_rules[rule];
      p0=1;
      for (i=0; i<ex_d->env[e].vars[j].nVal-1;i++)
      {
        ex_d->env[e].probs[ex_d->env[e].vars[j].firstBoolVar+i]=theta[i]/p0;
        p0=p0*(1-theta[i]/p0);
      }
    }
  }
  for (j=0;j<ex_d->nRules;j++)
  {
    free(Theta_rules[j]);
  }
  free(Theta_rules);
  PL_succeed;
}
static foreign_t rand_seed(term_t arg1)
{
  int seed;
  int ret;

  ret=PL_get_integer(arg1,&seed);
  RETURN_IF_FAIL

  srand((unsigned)seed);

  PL_succeed;
}

static foreign_t EM(term_t arg1,term_t arg2,term_t arg3,term_t arg4,term_t arg5,term_t arg6,term_t arg7,term_t arg8)
{
  term_t pterm,nil,out1,out2,out3,nodesTerm,ruleTerm,head,tail,pair,compoundTerm;
  DdNode * node1,**nodes_ex;
  int r,i,j,iter,cycle,ret;
  long iter1;
  size_t lenNodes;
  example_data * ex_d;
  double CLL0= -2.2*pow(10,10); //-inf
  double CLL1= -1.7*pow(10,8);  //+inf
  double p,p0,**eta_rule,ea,er;
  double ratio,diff;
  double **arrayprob; //new value of paramters after an iteration. One value ofr each rule and Bool var

  ret=PL_get_pointer(arg1,(void **)&ex_d);
  RETURN_IF_FAIL
  pair=PL_new_term_ref();
  head=PL_new_term_ref();
  nodesTerm=PL_copy_term_ref(arg2);

  ret=PL_skip_list(nodesTerm,0,&lenNodes);
  if (ret!=PL_LIST) return FALSE;

  out1=PL_new_term_ref();
  out2=PL_new_term_ref();
  out3=PL_new_term_ref();
  ruleTerm=PL_new_term_ref();
  tail=PL_new_term_ref();
  pterm=PL_new_term_ref();
  nil=PL_new_term_ref();
  compoundTerm=PL_new_term_ref();

  ret=PL_get_float(arg3,&ea);
  RETURN_IF_FAIL

  ret=PL_get_float(arg4,&er);
  RETURN_IF_FAIL
  ret=PL_get_integer(arg5,&iter);
  RETURN_IF_FAIL
  arrayprob=(double **) malloc(ex_d->nRules * sizeof(double *));
  for (j=0;j<ex_d->nRules;j++)
  {
    arrayprob[j]= (double *) malloc((ex_d->rules[j]-1)*sizeof(double));
  }

  nodes_ex=(DdNode **)malloc(lenNodes*sizeof(DdNode*));
  ex_d->nodes_probs=(double *)malloc(lenNodes*sizeof(double));
  ex_d->example_prob=(double *)malloc(lenNodes*sizeof(double));

  for (i=0;i<lenNodes;i++)
  {
    ret=PL_get_list(nodesTerm,pair,nodesTerm);
    RETURN_IF_FAIL
    ret=PL_get_list(pair,head,pair);
    RETURN_IF_FAIL
    ret=PL_get_pointer(head,(void **)&node1);
    RETURN_IF_FAIL
    nodes_ex[i]=node1;
    ret=PL_get_list(pair,head,pair);
    RETURN_IF_FAIL
    ret=PL_get_float(head,&(ex_d->example_prob[i]));
    RETURN_IF_FAIL
  }
  diff=CLL1-CLL0;
  ratio=diff/fabs(CLL0);
  if (iter==-1)
    iter1= 2147000000;
  else iter1=iter;

  cycle=0;
  while  ( (diff>ea) && (ratio>er) && (cycle<iter1) )
  {
    cycle++;
    for (r=0;r<ex_d->nRules;r++)
    {
      for (i=0;i<ex_d->rules[r]-1;i++)
      {
        eta_rule=ex_d->eta[r];
        eta_rule[i][0]=0;
        eta_rule[i][1]=0;
      }
    }
    CLL0 = CLL1;
    CLL1 = Expectation(ex_d,nodes_ex,lenNodes);
    Maximization(ex_d, arrayprob);
    diff=CLL1-CLL0;
    ratio=diff/fabs(CLL0);
  }
  ret=PL_put_nil(out2);
  RETURN_IF_FAIL
  for (r=0; r<ex_d->nRules; r++)
  {
    ret=PL_put_nil(tail);
    RETURN_IF_FAIL
    p0=1;
    for (i=0;i<ex_d->rules[r]-1;i++)
    {
      p=arrayprob[r][i]*p0;
      ret=PL_put_float(pterm,p);
      RETURN_IF_FAIL
      ret=PL_cons_list(tail,pterm,tail);
      RETURN_IF_FAIL
      p0=p0*(1-arrayprob[r][i]);
    }
    ret=PL_put_float(pterm,p0);
    RETURN_IF_FAIL
    ret=PL_cons_list(tail,pterm,tail);
    RETURN_IF_FAIL
    ret=PL_put_integer(ruleTerm,r);
    RETURN_IF_FAIL
    ret=PL_put_nil(nil);
    RETURN_IF_FAIL
    ret=PL_cons_list(tail,tail,nil);
    RETURN_IF_FAIL
    ret=PL_cons_list(compoundTerm,ruleTerm,tail);
    RETURN_IF_FAIL
    ret=PL_cons_list(out2,compoundTerm,out2);
    RETURN_IF_FAIL
  }
  ret=PL_put_nil(out3);
  RETURN_IF_FAIL
  for (i=0;i<lenNodes;i++)
  {
    ret=PL_put_float(pterm,ex_d->nodes_probs[i]);
    RETURN_IF_FAIL
    ret=PL_cons_list(out3,pterm,out3);
    RETURN_IF_FAIL
  }
  ret=PL_unify(out3,arg8);
  RETURN_IF_FAIL

  ret=PL_put_float(out1,CLL1);
  RETURN_IF_FAIL
  ret=PL_unify(out1,arg6);
  RETURN_IF_FAIL
  free(nodes_ex);
  free(ex_d->example_prob);
  free(ex_d->nodes_probs);

  return (PL_unify(out2,arg7));
}


/*

static int paths_to_non_zero(void)
{
  double paths;
  YAP_Term arg1,arg2,out;
  DdNode * node;

  arg1=YAP_ARG1;
  arg2=YAP_ARG2;
  node=(DdNode *)YAP_IntOfTerm(arg1);
  paths=Cudd_CountPathsToNonZero(node);
  out=YAP_MkFloatTerm(paths);
  return(YAP_Unify(out,arg2));
}

static int paths(void)
{
  double paths;
  YAP_Term arg1,arg2,out;
  DdNode * node;

  arg1=YAP_ARG1;
  arg2=YAP_ARG2;
  node=(DdNode *)YAP_IntOfTerm(arg1);
  paths=Cudd_CountPath(node);
  out=YAP_MkFloatTerm(paths);
  return(YAP_Unify(out,arg2));
}

static int dag_size(void)
{
  int size;
  YAP_Term arg1,arg2,out;
  DdNode * node;

  arg1=YAP_ARG1;
  arg2=YAP_ARG2;
  node=(DdNode *)YAP_IntOfTerm(arg1);
  size=Cudd_DagSize(node);
  out=YAP_MkIntTerm(size);
  return(YAP_Unify(out,arg2));
}
*/
install_t install()
/* function required by YAP for intitializing the predicates defined by a C function*/
{
  srand(10);

  PL_register_foreign("init",3,init,0);
  PL_register_foreign("init_bdd",2,init_bdd,0);
  PL_register_foreign("end",1,end,0);
  PL_register_foreign("end_bdd",1,end_bdd,0);
  PL_register_foreign("add_var",5,add_var,0);
  PL_register_foreign("add_query_var",5,add_query_var,0);
  PL_register_foreign("add_abd_var",5,add_abd_var,0);
  PL_register_foreign("equality",4,equality,0);
  PL_register_foreign("and",4,and,0);
  PL_register_foreign("one",2,one,0);
  PL_register_foreign("zero",2,zero,0);
  PL_register_foreign("or",4,or,0);
  PL_register_foreign("bdd_not",3,bdd_not,0);
  PL_register_foreign("create_dot",3,create_dot,0);
  PL_register_foreign("create_dot_string",3,create_dot_string,0);
  PL_register_foreign("init_test",2,init_test,0);
  PL_register_foreign("end_test",1,end_test,0);
  PL_register_foreign("ret_prob",3,ret_prob,0);
  PL_register_foreign("ret_abd_prob",4,ret_abd_prob,0);
  PL_register_foreign("ret_map_prob",4,ret_map_prob,0);
  PL_register_foreign("ret_vit_prob",4,ret_vit_prob,0);
  PL_register_foreign("reorder",1,reorder,0);
  PL_register_foreign("make_query_var",3,make_query_var,0);
  PL_register_foreign("em",8,EM,0);
  PL_register_foreign("randomize",1,randomize,0);
  PL_register_foreign("rand_seed",1,rand_seed,0);
//  PL_register_foreign("deref",1,rec_deref,0);
//  PL_register_foreign("garbage_collect",2,garbage_collect,0);
//  PL_register_foreign("bdd_to_add",2,bdd_to_add,0);
//  PL_register_foreign("paths_to_non_zero",2,paths_to_non_zero,0);
//  PL_register_foreign("paths",2,paths,0);
//  PL_register_foreign("dag_size",2,dag_size,0);
}
FILE * open_file(char *filename, const char *mode)
/* opens a file */
{
  FILE *fp;

  if ((fp = fopen(filename, mode)) == NULL)
  {
    perror(filename);
    exit(1);
  }
  return fp;
}


tablerow* init_table(int varcnt) {
  int i;
  tablerow *tab;

  tab = (tablerow *) malloc(sizeof(rowel) * varcnt);
  for (i = 0; i < varcnt; i++)
  {
    tab[i].row = NULL;
    tab[i].cnt = 0;
  }
  return tab;
}


void add_node(tablerow *tab, DdNode *node, double value) {
  int index = Cudd_NodeReadIndex(node);

  tab[index].row = (rowel *) realloc(tab[index].row,
    (tab[index].cnt + 1) * sizeof(rowel));
  tab[index].row[tab[index].cnt].key = node;
  tab[index].row[tab[index].cnt].value = value;
  tab[index].cnt += 1;
}

void add_or_replace_node(tablerow *tab, DdNode *node, double value)
{
  int i;
  int index = Cudd_NodeReadIndex(node);
  for(i = 0; i < tab[index].cnt; i++)
  {
    if (tab[index].row[i].key == node)
    {
      tab[index].row[i].value=value;
      return;
    }
  }
  tab[index].row = (rowel *) realloc(tab[index].row,
    (tab[index].cnt + 1) * sizeof(rowel));
  tab[index].row[tab[index].cnt].key = node;
  tab[index].row[tab[index].cnt].value = value;
  tab[index].cnt += 1;
}

double * get_value(tablerow *tab,  DdNode *node) {
  int i;
  int index = Cudd_NodeReadIndex(node);

  for(i = 0; i < tab[index].cnt; i++)
  {
    if (tab[index].row[i].key == node)
    {
      return &tab[index].row[i].value;
    }
  }
  return NULL;
}

void destroy_table(tablerow *tab,int varcnt)
{
  int i;

  for (i = 0; i < varcnt; i++)
  {
    free(tab[i].row);
  }
  free(tab);
}

expltablerow* expl_init_table(int varcnt) {
  int i;
  expltablerow *tab;

  tab = (expltablerow *) malloc(sizeof(explrowel) * varcnt);
  for (i = 0; i < varcnt; i++)
  {
    tab[i].row = NULL;
    tab[i].cnt = 0;
  }
  return tab;
}


void expl_add_node(expltablerow *tab, DdNode *node, int comp, prob_abd_expl value) {
  int index = Cudd_NodeReadIndex(node);

  tab[index].row = (explrowel *) realloc(tab[index].row,
    (tab[index].cnt + 1) * sizeof(explrowel));
  tab[index].row[tab[index].cnt].key.node = node;
  tab[index].row[tab[index].cnt].key.comp = comp;
  tab[index].row[tab[index].cnt].value = value;
  tab[index].cnt += 1;
}

prob_abd_expl * expl_get_value(expltablerow *tab,  DdNode *node, int comp) {
  int i;
  int index = Cudd_NodeReadIndex(node);

  for(i = 0; i < tab[index].cnt; i++)
  {
    if (tab[index].row[i].key.node == node &&
       tab[index].row[i].key.comp == comp)
    {
      return &tab[index].row[i].value;
    }
  }
  return NULL;
}

void expl_destroy_table(expltablerow *tab,int varcnt)
{
  int i,j;

  for (i = 0; i < varcnt; i++)
  {
    for (j = 0; j< tab[i].cnt; j++)
    {
      free_list(tab[i].row[j].value.mpa);
    }
    free(tab[i].row);
  }
  free(tab);
}
