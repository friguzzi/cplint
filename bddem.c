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
#include "cuddInt.h"
#include <SWI-Prolog.h>
#define LOGZERO log(0.01)
#define CACHE_SLOTS 1 
#define UNIQUE_SLOTS 1


typedef struct
{
  int nVal,nRule;
  int firstBoolVar;
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

tablerow * table;
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
  tablerow * nodesF; // tables of probabilities for nodes in Forward step
  double * example_prob; // probability (frequency) of examples in the data
} example_data;


static foreign_t ret_prob(term_t,term_t,term_t);
double Prob(DdNode *node,environment *env,tablerow *table,int comp_par);
static foreign_t end_bdd(term_t);
static foreign_t init_test(term_t, term_t);
static foreign_t add_var(term_t,term_t,term_t,term_t,term_t);
static foreign_t init(term_t,term_t,term_t);
static foreign_t end(term_t);
static foreign_t EM(term_t,term_t,term_t,term_t,
  term_t,term_t,term_t,term_t,term_t);
double ProbPath(example_data * ex_d,DdNode *node, int comp_par, int nex);
//static int rec_deref(void);
void Forward(example_data * ex_d,DdNode *node, int nex);
void UpdateForward(example_data * ex_d,DdNode * node, int nex,
  DdNode *** nodesToVisit,int * NnodesToVisit);
double GetOutsideExpe(example_data *ex_d,DdNode *root,double ex_prob, int nex);
void Maximization(example_data * ex_d, double ** arrayprob);
static double Expectation(example_data *ex_d,DdNode **nodes_ex, int lenNodes);
FILE *open_file(char *filename, const char *mode);
tablerow* init_table(int varcnt);
double * get_value(tablerow *tab,  DdNode *node);
void add_or_replace_node(tablerow *tab, DdNode *node, double value);
void add_node(tablerow *tab, DdNode *node, double value);
void destroy_table(tablerow *tab,int varcnt);
install_t install(void);

static foreign_t init(term_t arg1,term_t arg2,term_t arg3)
{
  int j,i;
  example_data * ex_d; 
  double ***eta;
  double ***eta_temp;
  int nRules, *rules;
  term_t ex_d_t=PL_new_term_ref();
  term_t list=PL_copy_term_ref(arg2);
  term_t head=PL_new_term_ref();
  
  ex_d=(example_data *)malloc(sizeof(example_data));

  ex_d->ex=0;
  PL_get_integer(arg1,&nRules);
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
    PL_get_list(list,head,list);
    PL_get_integer(head,&rules[j]);
    eta[j]= (double **) malloc((rules[j]-1)*sizeof(double *));
    eta_temp[j]= (double **) malloc((rules[j]-1)*sizeof(double *));
    for (i=0;i<rules[j]-1;i++)
    {
      eta[j][i]=(double *) malloc(2*sizeof(double));
      eta_temp[j][i]=(double *) malloc(2*sizeof(double));
    }
  }
  PL_put_integer(ex_d_t,(long) ex_d);
  return(PL_unify(ex_d_t,arg3));

}

static foreign_t init_bdd(term_t arg1, term_t arg2)  
{
  example_data * ex_d;
  DdManager * mgr;
  term_t env_t;
  long ex_d_l;
  int ex;

  env_t=PL_new_term_ref();
  PL_get_long(arg1,&ex_d_l);
  ex_d=(example_data *)ex_d_l;
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

  PL_put_integer(env_t,(long) (ex_d->env+ex));
  return(PL_unify(env_t,arg2));
 
}

static foreign_t end_bdd(term_t arg1)
{
  long ex_d_l;
  example_data *ex_d;

  PL_get_long(arg1,&ex_d_l);
  ex_d=(example_data *) ex_d_l;
  ex_d->ex=ex_d->ex+1;
  PL_succeed;
}

static foreign_t init_test(term_t arg1,term_t arg2)
{
  term_t env_t;
  environment * env;
  int nRules;
  PL_get_integer(arg1,&nRules);
  env_t=PL_new_term_ref();
  
  env=(environment *)malloc(sizeof(environment));
  env->mgr=Cudd_Init(0,0,UNIQUE_SLOTS,CACHE_SLOTS,5120);
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
  PL_put_integer(env_t,(long) env);
  return(PL_unify(env_t,arg2));
}

static foreign_t end_test(term_t arg1)
{
  long env_l;
  environment *env;

  PL_get_long(arg1,&env_l);
  env=(environment *)env_l;
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
      ex_d->nodesF=init_table(ex_d->env[i].boolVars);
      
      Forward(ex_d,nodes_ex[i],i);
      rootProb=GetOutsideExpe(ex_d,nodes_ex[i],ex_d->example_prob[i],i);

      if (rootProb<=0.0)
        CLL = CLL + LOGZERO*ex_d->example_prob[i];
      else
        CLL = CLL + log(rootProb)*ex_d->example_prob[i];
      
      ex_d->nodes_probs[i]=rootProb;
      destroy_table(ex_d->nodesB,ex_d->env[i].boolVars);
      destroy_table(ex_d->nodesF,ex_d->env[i].boolVars);
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
  int r,i;
  long ex_d_l;
  example_data * ex_d;
  PL_get_long(arg1,&ex_d_l);
  ex_d=(example_data *)ex_d_l;
 
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
  long nodeint,env_l;
  environment * env;
  DdNode * node;
  tablerow * table;

  PL_get_long(arg1,&env_l);
  env=(environment *)env_l;
  PL_get_long(arg2,&nodeint);
  node=(DdNode *)nodeint;
  out=PL_new_term_ref();
 
  if (!Cudd_IsConstant(node))
  {
    table=init_table(env->boolVars);
    PL_put_float(out,Prob(node,env,table,0));
    destroy_table(table,env->boolVars);
  }
  else
  {
    if (node==Cudd_ReadOne(env->mgr))
      PL_put_float(out,1.0);
    else  
      PL_put_float(out,0.0);
  }

  return(PL_unify(out,arg3));
}

double Prob(DdNode *node, environment * env, tablerow * table,int comp_par)
/* compute the probability of the expression rooted at node.
table is used to store nodeB for which the probability has alread been computed
so that it is not recomputed
 */
{
  int index,mVarIndex,comp,pos;
  variable v;
  double res;
  double p,pt,pf,BChild0,BChild1;
  double * value_p;
  DdNode *nodekey,*T,*F;

  comp=Cudd_IsComplement(node);   
  comp=(comp && !comp_par) ||(!comp && comp_par);
  if (Cudd_IsConstant(node))
  {
    if (comp)
      return 0.0;
    else
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
      pf=Prob(F,env,table,comp);
      pt=Prob(T,env,table,comp);
      BChild0=pf*(1-p);
      BChild1=pt*p;
      mVarIndex=env->bVar2mVar[index];
      v=env->vars[mVarIndex];
      res=BChild0+BChild1;
      add_node(table,nodekey,res);
      return res;
    }
  }
}



static foreign_t add_var(term_t arg1,term_t arg2,term_t arg3,term_t arg4, term_t arg5)
{
  term_t out,head,probTerm;
  variable * v;
  int i;
  DdNode * node;
  double p,p0;
  long env_l;
  environment * env;

  head=PL_new_term_ref();
  out=PL_new_term_ref();
  PL_get_long(arg1,&env_l);
  env=(environment *)env_l;
  env->nVars=env->nVars+1;
  env->vars=(variable *) realloc(env->vars,env->nVars * sizeof(variable));

  v=&env->vars[env->nVars-1];
  PL_get_integer(arg2,&v->nVal);
  PL_get_integer(arg4,&v->nRule);

  v->firstBoolVar=env->boolVars;
  env->probs=(double *) realloc(env->probs,(((env->boolVars+v->nVal-1)* sizeof(double))));
  env->bVar2mVar=(int *) realloc(env->bVar2mVar,((env->boolVars+v->nVal-1)* sizeof(int)));
  probTerm=PL_copy_term_ref(arg3); 
  p0=1;
  for (i=0;i<v->nVal-1;i++)
  {
    node=Cudd_bddIthVar(env->mgr,env->boolVars+i);
    PL_get_list(probTerm,head,probTerm);
    PL_get_float(head,&p);
    env->bVar2mVar[env->boolVars+i]=env->nVars-1;
    env->probs[env->boolVars+i]=p/p0;
    p0=p0*(1-p/p0);
  }
  env->boolVars=env->boolVars+v->nVal-1;
  env->rules[v->nRule]= v->nVal; 

  PL_put_integer(out,env->nVars-1);

  return(PL_unify(out,arg5));
}

static foreign_t equality(term_t arg1,term_t arg2,term_t arg3, term_t arg4)
{
  term_t out;
  int varIndex;
  int value;
  int i;
  variable v;
  DdNode * node, * tmp,*var;
  long env_l;
  environment * env;

  PL_get_long(arg1,&env_l);
  env=(environment *)env_l;

  PL_get_integer(arg2,&varIndex);
  PL_get_integer(arg3,&value);
  v=env->vars[varIndex];
  i=v.firstBoolVar;
  tmp=Cudd_ReadOne(env->mgr);
  Cudd_Ref(tmp);
  node=NULL;
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
  out=PL_new_term_ref();
  PL_put_integer(out,(long) node);
  return(PL_unify(out,arg4));
}

static foreign_t one(term_t arg1, term_t arg2)
{
  term_t out;
  DdNode * node;
  long env_l;
  environment *env;
  int res;

  PL_get_long(arg1,&env_l);
  env=(environment *)env_l;

  node =  Cudd_ReadOne(env->mgr);
  Cudd_Ref(node);
  out=PL_new_term_ref();
  PL_put_integer(out,(long) node);
  res=PL_unify(out,arg2);
  return res;

//  return(PL_unify(out,arg2));
}

static foreign_t zero(term_t arg1, term_t arg2)
{
  term_t out;
  DdNode * node;
  long env_l;
  environment *env;

  PL_get_long(arg1,&env_l);
  env=(environment *)env_l;

  node = Cudd_ReadLogicZero(env->mgr);
  Cudd_Ref(node);
  out=PL_new_term_ref();
  PL_put_integer(out,(long) node);
  return(PL_unify(out,arg2));
}

static foreign_t bdd_not(term_t arg1,term_t arg2, term_t arg3)
{
  term_t out;
  long nodeint;
  DdNode * node;
  long env_l;
  environment *env;

  PL_get_long(arg1,&env_l);
  env=(environment *)env_l;

  PL_get_long(arg2,&nodeint);
  node = (DdNode *)nodeint;
  node=Cudd_Not(node);
  out=PL_new_term_ref();
  PL_put_integer(out,(long) node);
  return(PL_unify(out,arg3));
}

static foreign_t and(term_t arg1,term_t arg2,term_t arg3, term_t arg4)
{
  term_t out;
  DdNode * node1, *node2,*nodeout;
  long node1int,node2int;
  long env_l;
  environment *env;
  int res;
  PL_get_long(arg1,&env_l);
  env=(environment *)env_l;

  PL_get_long(arg2,&node1int);
  node1 = (DdNode *)node1int;
  PL_get_long(arg3,&node2int);
  node2 = (DdNode *)node2int;
  nodeout=Cudd_bddAnd(env->mgr,node1,node2);
  Cudd_Ref(nodeout);
  out=PL_new_term_ref();
  PL_put_integer(out,(long) nodeout);
  res=PL_unify(out,arg4);
  return res;
}

static foreign_t or(term_t arg1,term_t arg2,term_t arg3, term_t arg4)
{
  term_t out;
  DdNode * node1, *node2,*nodeout;
  long node1int,node2int;
  long env_l;
  environment *env;

  PL_get_long(arg1,&env_l);
  env=(environment *)env_l;

  PL_get_long(arg2,&node1int);
  node1 = (DdNode *)node1int;
  PL_get_long(arg3,&node2int);
  node2 = (DdNode *)node2int;

  nodeout=Cudd_bddOr(env->mgr,node1,node2);
  Cudd_Ref(nodeout);
  out=PL_new_term_ref();
  PL_put_integer(out,(long) nodeout);
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

static int create_dot(void)
{
  char * onames[]={"Out"};
  char ** inames;
   DdNode * array[1];
  YAP_Term arg1,arg2;
  int i,b,index;
  variable v;
  char numberVar[10],numberBit[10],filename[1000];
  FILE * file;
  
  arg1=YAP_ARG1;
  arg2=YAP_ARG2;

  YAP_StringToBuffer(arg2,filename,1000);
  inames= (char **) malloc(sizeof(char *)*(boolVars_ex[ex]));
  index=0;
  for (i=0;i<nVars_ex[ex];i++)
  {
    v=vars_ex[ex][i];
    for (b=0;b<v.nVal-1;b++)
    {  
      inames[b+index]=(char *) malloc(sizeof(char)*20);
      strcpy(inames[b+index],"X");
      sprintf(numberVar,"%d",i);
      strcat(inames[b+index],numberVar);
      strcat(inames[b+index],"_");
      sprintf(numberBit,"%d",b);
      strcat(inames[b+index],numberBit);
    }
    index=index+v.nVal-1;
  }
  array[0]=(DdNode *)YAP_IntOfTerm(arg1);
  file = open_file(filename, "w");
  Cudd_DumpDot(mgr_ex[ex],1,array,inames,onames,file);
  fclose(file);
  index=0;
  for (i=0;i<nVars_ex[ex];i++)
  {
    v=vars_ex[ex][i];
    for (b=0;b<v.nVal-1;b++)
    {  
      free(inames[b+index]);
    }
    index=index+v.nVal-1;
  }
  free(inames);
  return 1;
}


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

double ProbPath(example_data * ex_d,DdNode *node,int comp_par, int nex)
{
  int index,mVarIndex,comp,pos,position;//,boolVarIndex;
  variable v;
  double res;
  double p,pt,pf,BChild0,BChild1,e0,e1;
  double * value_p,** eta_rule;
  DdNode *nodekey,*T,*F;

  comp=Cudd_IsComplement(node);
  comp=(comp && !comp_par) ||(!comp && comp_par);
  if (Cudd_IsConstant(node))
  {
    if (comp)
    {
      return 0.0;
    }
    else
    {
      return 1.0;
    }
  }
  else
  {
    nodekey=Cudd_Regular(node);
    value_p=get_value(ex_d->nodesB,nodekey);
    if (value_p!=NULL)
    {
      return *value_p;
    }
    else
    {
      index=Cudd_NodeReadIndex(node);
      p=ex_d->env[nex].probs[index];
      T = Cudd_T(node);
      F = Cudd_E(node);
      pf=ProbPath(ex_d,F,comp,nex);
      pt=ProbPath(ex_d,T,comp,nex);
      BChild0=pf*(1-p);
      BChild1=pt*p;
      value_p=get_value(ex_d->nodesF,nodekey);
      e0 = (*value_p)*BChild0; 
      e1 = (*value_p)*BChild1; 
      mVarIndex=ex_d->env[nex].bVar2mVar[index];
      v=ex_d->env[nex].vars[mVarIndex];
      pos=index-v.firstBoolVar;
      eta_rule=ex_d->eta_temp[v.nRule];
      eta_rule[pos][0]=eta_rule[pos][0]+e0;
      eta_rule[pos][1]=eta_rule[pos][1]+e1;
      res=BChild0+BChild1;
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
  int * NnodesToVisit;

  environment env;
  int i,j;
  env=ex_d->env[nex];
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
    add_node(ex_d->nodesF,Cudd_Regular(root),1);
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
  else
  {
    add_node(ex_d->nodesF,Cudd_Regular(root),1);
  }
}

void UpdateForward(example_data *ex_d,DdNode *node, int nex,
  DdNode *** nodesToVisit, int * NnodesToVisit)
{
  int index,position,mVarIndex;
  DdNode *T,*E,*nodereg;
  double *value_p,*value_p_T,*value_p_F,p;

  if (Cudd_IsConstant(node)) 
  {
    return;
  }
  else
  {
    index=Cudd_NodeReadIndex(node);
    mVarIndex=ex_d->env[nex].bVar2mVar[index];
   // v=ex_d->env[nex].vars[mVarIndex];
    p=ex_d->env[nex].probs[index];
    nodereg=Cudd_Regular(node);
    value_p=get_value(ex_d->nodesF,nodereg);
    if (value_p== NULL)
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
        value_p_T=get_value(ex_d->nodesF,T);
        if (value_p_T!= NULL)
        {
           *value_p_T= *value_p_T+*value_p*p;
        }
        else
        {
          add_or_replace_node(ex_d->nodesF,Cudd_Regular(T),*value_p*p);
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
        value_p_F=get_value(ex_d->nodesF,Cudd_Regular(E));

        if (value_p_F!= NULL)
        {
          *value_p_F= *value_p_F+*value_p*(1-p);
        }
        else
        {
          add_or_replace_node(ex_d->nodesF,Cudd_Regular(E),*value_p*(1-p));
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
  int i,j,mVarIndex,bVarIndex;
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
  rootProb=ProbPath(ex_d,root,0,nex);
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
      for (i=0; i<ex_d->env[nex].vars[mVarIndex].nVal-1;i++)
      {
        theta=ex_d->env[nex].probs[bVarIndex];
        eta_rule[i][0]=eta_rule[i][0]+T*(1-theta);
        eta_rule[i][1]=eta_rule[i][1]+T*theta;
      }   
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
  int i,j,e,rule;
  double * theta,p0;
  double pmass,par;
  double **Theta_rules;
  long ex_d_l;
  example_data * ex_d;

  PL_get_long(arg1,&ex_d_l);
  ex_d=(example_data *)ex_d_l;
   
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

static foreign_t EM(term_t arg1,term_t arg2,term_t arg3,term_t arg4,term_t arg5,term_t arg6,term_t arg7,term_t arg8,term_t arg9)
{
  term_t pterm,nil,out1,out2,out3,nodesTerm,ruleTerm,head,tail,pair,compoundTerm;
  DdNode * node1,**nodes_ex;
  int r,lenNodes,i,j,iter,cycle;
  long node1int;
  long iter1;
  long ex_d_l;
  example_data * ex_d;
  double CLL0= -2.2*pow(10,10); //-inf
  double CLL1= -1.7*pow(10,8);  //+inf   
  double p,p0,**eta_rule,ea,er; 
  double ratio,diff;
  double **arrayprob; //new value of paramters after an iteration. One value ofr each rule and Bool var

  PL_get_long(arg1,&ex_d_l);
  ex_d=(example_data *)ex_d_l;
  pair=PL_new_term_ref();
  head=PL_new_term_ref();
  nodesTerm=PL_copy_term_ref(arg2);
  out1=PL_new_term_ref();
  out2=PL_new_term_ref();
  out3=PL_new_term_ref();
  ruleTerm=PL_new_term_ref();
  tail=PL_new_term_ref();
  pterm=PL_new_term_ref();
  nil=PL_new_term_ref();
  compoundTerm=PL_new_term_ref();  

  PL_get_float(arg3,&ea);
  PL_get_float(arg4,&er);
  PL_get_integer(arg5,&lenNodes);  
  PL_get_integer(arg6,&iter);
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
    PL_get_list(nodesTerm,pair,nodesTerm);
    PL_get_list(pair,head,pair);
    //printf("qui\n");
    PL_get_long(head,&node1int);
    //printf("qua\n");
    node1=(DdNode *)node1int;
    nodes_ex[i]=node1;
    PL_get_list(pair,head,pair);
    PL_get_float(head,&(ex_d->example_prob[i]));
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
  PL_put_nil(out2);
  for (r=0; r<ex_d->nRules; r++)
  {
    PL_put_nil(tail);
    p0=1;
    for (i=0;i<ex_d->rules[r]-1;i++)
    {
      p=arrayprob[r][i]*p0;
      PL_put_float(pterm,p);
      PL_cons_list(tail,pterm,tail);
      p0=p0*(1-arrayprob[r][i]);
    }
    PL_put_float(pterm,p0);
    PL_cons_list(tail,pterm,tail);
    PL_put_integer(ruleTerm,r);
    PL_put_nil(nil);
    PL_cons_list(tail,tail,nil);
    PL_cons_list(compoundTerm,ruleTerm,tail);
    PL_cons_list(out2,compoundTerm,out2);
  }
  PL_put_nil(out3);
  for (i=0;i<lenNodes;i++)
  {
    PL_put_float(pterm,ex_d->nodes_probs[i]);
    PL_cons_list(out3,pterm,out3);
  }
  PL_unify(out3,arg9);

  PL_put_float(out1,CLL1);
  PL_unify(out1,arg7);
  free(nodes_ex);
  free(ex_d->example_prob);
  free(ex_d->nodes_probs);

  return (PL_unify(out2,arg8));
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
  PL_register_foreign("equality",4,equality,0);
  PL_register_foreign("and",4,and,0);
  PL_register_foreign("one",2,one,0);
  PL_register_foreign("zero",2,zero,0);
  PL_register_foreign("or",4,or,0);
  PL_register_foreign("bdd_not",3,bdd_not,0);
//  PL_register_foreign("create_dot",2,create_dot,0);
  PL_register_foreign("init_test",2,init_test,0);
  PL_register_foreign("end_test",1,end_test,0);
  PL_register_foreign("ret_prob",3,ret_prob,0);
  PL_register_foreign("em",9,EM,0);
  PL_register_foreign("randomize",1,randomize,0);
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
