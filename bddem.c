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
#include "YapInterface.h"
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

static variable ** vars_ex;
static int ** bVar2mVar_ex;
static double * sigma;
static double ***eta;
static double ***eta_temp;
static double **arrayprob;
static int *rules;
static DdManager **mgr_ex;
static int *nVars_ex;
static int nRules;
double * nodes_probs_ex;
double ** probs_ex;
static int * boolVars_ex; 
tablerow * nodesB;
tablerow * nodesF;
int ex,cycle;
DdNode *** nodesToVisit;
int * NnodesToVisit;
double * example_prob;
static int ret_prob(void);
double Prob(DdNode *node,int comp_par);
static int end_bdd(void);
static int init_test(void);
static int add_var(void);
static int init(void);
static int end(void);
static int EM(void);
static int Q(void);
double ProbPath(DdNode *node, int comp_par, int nex);
static int rec_deref(void);
int indexMvar(DdNode *node);
void Forward(DdNode *node, int nex);
void GetForward(DdNode *node, double ForwProbPath);
void UpdateForward(DdNode * node, int nex);
double GetOutsideExpe(DdNode *root,double ex_prob, int nex);
void Maximization(void);
static double Expectation(DdNode **nodes_ex, int lenNodes);
void init_my_predicates(void);
FILE *open_file(char *filename, const char *mode);
tablerow* init_table(int varcnt);
double * get_value(tablerow *tab,  DdNode *node);
void add_or_replace_node(tablerow *tab, DdNode *node, double value);
void add_node(tablerow *tab, DdNode *node, double value);
void destroy_table(tablerow *tab,int varcnt);

static int init(void)
{
  int j,i;
  YAP_Term arg1,arg2,list;

  ex=0;
  cycle=0;
  arg1=YAP_ARG1;
  arg2=YAP_ARG2;
  nRules=YAP_IntOfTerm(arg1);

  vars_ex=NULL;
  nVars_ex=NULL;
  eta= (double ***) malloc(nRules * sizeof(double **));
  eta_temp= (double ***) malloc(nRules * sizeof(double **));
  rules= (int *) malloc(nRules * sizeof(int));
  arrayprob=(double **) malloc(nRules * sizeof(double *));
  probs_ex=NULL;
  bVar2mVar_ex=NULL;
  boolVars_ex=NULL;
  mgr_ex=NULL;
  nodes_probs_ex=NULL;
  list=arg2;
  for (j=0;j<nRules;j++)  
  {
    rules[j]=YAP_IntOfTerm(YAP_HeadOfTerm(list));
    list=YAP_TailOfTerm(list);
    eta[j]= (double **) malloc((rules[j]-1)*sizeof(double *));
    eta_temp[j]= (double **) malloc((rules[j]-1)*sizeof(double *));
    arrayprob[j]= (double *) malloc((rules[j]-1)*sizeof(double));
    for (i=0;i<rules[j]-1;i++)
    {
      eta[j][i]=(double *) malloc(2*sizeof(double));
      eta_temp[j][i]=(double *) malloc(2*sizeof(double));
    }
  }
  return 1;
}

static int init_bdd(void)  
{
  mgr_ex=(DdManager **) realloc(mgr_ex, (ex+1)* sizeof(DdManager *)); 
  mgr_ex[ex]=Cudd_Init(0,0,UNIQUE_SLOTS,CACHE_SLOTS,5120);
  Cudd_AutodynEnable(mgr_ex[ex], CUDD_REORDER_GROUP_SIFT);
  Cudd_SetMaxCacheHard(mgr_ex[ex], 0);
  Cudd_SetLooseUpTo(mgr_ex[ex], 0);
  Cudd_SetMinHit(mgr_ex[ex], 15);

  bVar2mVar_ex=(int **) realloc(bVar2mVar_ex, (ex+1)* sizeof(int *));
  bVar2mVar_ex[ex]=NULL;

  vars_ex=(variable **) realloc(vars_ex, (ex+1)* sizeof(variable *));
  vars_ex[ex]=NULL;
  
  nVars_ex=(int *) realloc(nVars_ex, (ex+1)* sizeof(int ));
  nVars_ex[ex]=0;

  probs_ex=(double **) realloc(probs_ex, (ex+1)* sizeof(double *)); 
  probs_ex[ex]=NULL;
  
  boolVars_ex=(int *) realloc(boolVars_ex, (ex+1)* sizeof(int ));
  boolVars_ex[ex]=0;

  return 1;
}

static int end_bdd(void)
{

  ex=ex+1;
  return 1;
}



static int init_test(void)
{
  YAP_Term arg1;  

  arg1=YAP_ARG1;
  nRules=YAP_IntOfTerm(arg1);

  ex=0;
  mgr_ex=(DdManager **) malloc((ex+1)* sizeof(DdManager *));
  mgr_ex[ex]=Cudd_Init(0,0,UNIQUE_SLOTS,CACHE_SLOTS,5120);
  Cudd_AutodynEnable(mgr_ex[ex], CUDD_REORDER_GROUP_SIFT);
  Cudd_SetMaxCacheHard(mgr_ex[ex], 0);
  Cudd_SetLooseUpTo(mgr_ex[ex], 0);
  Cudd_SetMinHit(mgr_ex[ex], 15);

  bVar2mVar_ex=(int **) malloc((ex+1)* sizeof(int *));
  bVar2mVar_ex[ex]=NULL;

  vars_ex=(variable **) malloc((ex+1)* sizeof(variable *));
  vars_ex[ex]=NULL;

  nVars_ex=(int *) malloc((ex+1)* sizeof(int ));
  nVars_ex[ex]=0;

  probs_ex=(double **) malloc((ex+1)* sizeof(double *));
  probs_ex[ex]=NULL;

  boolVars_ex=(int *) malloc((ex+1)* sizeof(int ));
  boolVars_ex[ex]=0;

  rules= (int *) malloc(nRules * sizeof(int));

  return 1;

}

static int end_test(void)
{
  free(bVar2mVar_ex[ex]);
  free(vars_ex[ex]);
  Cudd_Quit(mgr_ex[ex]);
  free(probs_ex[ex]);
  free(rules);
  free(mgr_ex);
  free(bVar2mVar_ex);
  free(vars_ex);
  free(probs_ex);
  free(nVars_ex);
  free(boolVars_ex);

  return 1;
}



static double Expectation(DdNode **nodes_ex,int lenNodes)
{
  int i;
  double rootProb,CLL=0;

  for(i=0;i<lenNodes;i++)  
  {
    if (!Cudd_IsConstant(nodes_ex[i]))
    {
      nodesB=init_table(boolVars_ex[i]);
      nodesF=init_table(boolVars_ex[i]);
      
      Forward(nodes_ex[i],i);
      rootProb=GetOutsideExpe(nodes_ex[i],example_prob[i],i);

      if (rootProb<=0.0)
        CLL = CLL + LOGZERO*example_prob[i];
      else
        CLL = CLL + log(rootProb)*example_prob[i];
      
      nodes_probs_ex[i]=rootProb;
      destroy_table(nodesB,boolVars_ex[i]);
      destroy_table(nodesF,boolVars_ex[i]);
    }
    else
      if (nodes_ex[i]==Cudd_ReadLogicZero(mgr_ex[i]))
      {
        CLL=CLL+LOGZERO*example_prob[i];
	nodes_probs_ex[i]=0.0;
      }
      else
        nodes_probs_ex[i]=1.0;
  }
  return CLL;
}

static int end(void)
{
  int r,i;

  for (i=0;i<ex;i++)
  {
    Cudd_Quit(mgr_ex[i]);
    free(bVar2mVar_ex[i]);
    free(vars_ex[i]);
    free(probs_ex[i]);
    fflush(stdout);
  }
  
  free(mgr_ex);
  free(bVar2mVar_ex);
  free(vars_ex);
  free(probs_ex);
  free(nVars_ex);
  free(boolVars_ex);
  for (r=0;r<nRules;r++)
  {
    for (i=0;i<rules[r]-1;i++)
    {
      free(eta[r][i]);
      free(eta_temp[r][i]);
    }
    free(eta[r]);
    free(eta_temp[r]);
  }
  free(eta);
  free(eta_temp);
  for (r=0;r<nRules;r++)
  {
    free(arrayprob[r]);
  }
  free(arrayprob);
  free(rules);

  return 1;
}


static int ret_prob(void)
{
  YAP_Term arg1,arg2,out;
  DdNode * node;
  
  arg1=YAP_ARG1;
  arg2=YAP_ARG2;
  node=(DdNode *)YAP_IntOfTerm(arg1);

  if (!Cudd_IsConstant(node))
  {
    table=init_table(boolVars_ex[ex]);
    out=YAP_MkFloatTerm(Prob(node,0));
    destroy_table(table,boolVars_ex[ex]);
  }
  else
  {
    if (node==Cudd_ReadOne(mgr_ex[ex]))
      out=YAP_MkFloatTerm(1.0);
    else  
      out=YAP_MkFloatTerm(0.0);
  }

  return(YAP_Unify(out,arg2));
}

double Prob(DdNode *node,int comp_par)
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
      p=probs_ex[ex][index];
      T = Cudd_T(node);
      F = Cudd_E(node);
      pf=Prob(F,comp);
      pt=Prob(T,comp);
      BChild0=pf*(1-p);
      BChild1=pt*p;
      mVarIndex=bVar2mVar_ex[ex][index];
      v=vars_ex[ex][mVarIndex];
      pos=index-v.firstBoolVar;
      res=BChild0+BChild1;
      add_node(table,nodekey,res);
      return res;
    }
  }
}



static int add_var(void)
{
  YAP_Term arg1,arg2,arg3,arg4,out,probTerm,probTerm_temp;
  variable * v;
  int i;
  DdNode * node;
  double p,p0;


  arg1=YAP_ARG1;
  arg2=YAP_ARG2;
  arg3=YAP_ARG3; 
  arg4=YAP_ARG4;
  nVars_ex[ex]=nVars_ex[ex]+1;
  vars_ex[ex]=(variable *) realloc(vars_ex[ex],nVars_ex[ex] * sizeof(variable));

  v=&vars_ex[ex][nVars_ex[ex]-1];
  v->nVal=YAP_IntOfTerm(arg1);
  v->nRule=YAP_IntOfTerm(arg3);
  v->firstBoolVar=boolVars_ex[ex];
  probs_ex[ex]=(double *) realloc(probs_ex[ex],(((boolVars_ex[ex]+v->nVal-1)* sizeof(double))));
  bVar2mVar_ex[ex]=(int *) realloc(bVar2mVar_ex[ex],((boolVars_ex[ex]+v->nVal-1)* sizeof(int)));
  probTerm=arg2; 
  p0=1;
  for (i=0;i<v->nVal-1;i++)
  {
    node=Cudd_bddIthVar(mgr_ex[ex],boolVars_ex[ex]+i);
    p=YAP_FloatOfTerm(YAP_HeadOfTerm(probTerm));
    bVar2mVar_ex[ex][boolVars_ex[ex]+i]=nVars_ex[ex]-1;
    probs_ex[ex][boolVars_ex[ex]+i]=p/p0;
    probTerm_temp=YAP_TailOfTerm(probTerm);
    probTerm=probTerm_temp;
    p0=p0*(1-p/p0);
  }
  boolVars_ex[ex]=boolVars_ex[ex]+v->nVal-1;
  rules[v->nRule]= v->nVal; 
  out=YAP_MkIntTerm((YAP_Int) nVars_ex[ex]-1);
  return YAP_Unify(out,arg4);
}

static int equality(void)
{
  YAP_Term arg1,arg2,arg3,out;
  int varIndex;
  int value;
  int i;
  variable v;
  DdNode * node, * tmp,*var;

  arg1=YAP_ARG1;
  arg2=YAP_ARG2;
  arg3=YAP_ARG3;
  varIndex=YAP_IntOfTerm(arg1);
  value=YAP_IntOfTerm(arg2);
  v=vars_ex[ex][varIndex];
  i=v.firstBoolVar;
  tmp=Cudd_ReadOne(mgr_ex[ex]);
  Cudd_Ref(tmp);
  node=NULL;
  for (i=v.firstBoolVar;i<v.firstBoolVar+value;i++)
  {
    var=Cudd_bddIthVar(mgr_ex[ex],i);
    node=Cudd_bddAnd(mgr_ex[ex],tmp,Cudd_Not(var));
    Cudd_Ref(node);
    Cudd_RecursiveDeref(mgr_ex[ex],tmp);
    tmp=node;
  }
  if (!(value==v.nVal-1))
  {
    var=Cudd_bddIthVar(mgr_ex[ex],v.firstBoolVar+value);
    node=Cudd_bddAnd(mgr_ex[ex],tmp,var);
    Cudd_Ref(node);
    Cudd_RecursiveDeref(mgr_ex[ex],tmp);
  }
  out=YAP_MkIntTerm((YAP_Int) node);
  return(YAP_Unify(out,arg3));
}

static int one(void)
{
  YAP_Term arg,out;
  DdNode * node;

  arg=YAP_ARG1;
  node =  Cudd_ReadOne(mgr_ex[ex]);
  Cudd_Ref(node);
  out=YAP_MkIntTerm((YAP_Int) node);
  return(YAP_Unify(out,arg));
}

static int zero(void)
{
  YAP_Term arg,out;
  DdNode * node;

  arg=YAP_ARG1;
  node = Cudd_ReadLogicZero(mgr_ex[ex]);
  Cudd_Ref(node);
  out=YAP_MkIntTerm((YAP_Int) node);
  return(YAP_Unify(out,arg));
}

static int bdd_not(void)
{
  YAP_Term arg1,arg2,out;
  DdNode * node;
  arg1=YAP_ARG1;
  arg2=YAP_ARG2;

  node = (DdNode *)YAP_IntOfTerm(arg1);
  node=Cudd_Not(node);
  out=YAP_MkIntTerm((YAP_Int) node);
  return(YAP_Unify(out,arg2));
}

static int and(void)
{
  YAP_Term arg1,arg2,arg3,out;
  DdNode * node1, *node2,*nodeout;

  arg1=YAP_ARG1;
  arg2=YAP_ARG2;
  arg3=YAP_ARG3;
  node1=(DdNode *)YAP_IntOfTerm(arg1);
  node2=(DdNode *)YAP_IntOfTerm(arg2);
  nodeout=Cudd_bddAnd(mgr_ex[ex],node1,node2);
  Cudd_Ref(nodeout);
  out=YAP_MkIntTerm((YAP_Int) nodeout);
  return(YAP_Unify(out,arg3));
}

static int or(void)
{
  YAP_Term arg1,arg2,arg3,out;
  DdNode * node1,*node2,*nodeout;

  arg1=YAP_ARG1;
  arg2=YAP_ARG2;
  arg3=YAP_ARG3;
  node1=(DdNode *)YAP_IntOfTerm(arg1);
  node2=(DdNode *)YAP_IntOfTerm(arg2);
  nodeout=Cudd_bddOr(mgr_ex[ex],node1,node2);
  Cudd_Ref(nodeout);
  out=YAP_MkIntTerm((YAP_Int) nodeout);
  return(YAP_Unify(out,arg3));
}

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



double ProbPath(DdNode *node,int comp_par, int nex)
{
  int index,mVarIndex,comp,pos,position,boolVarIndex;
  variable v;
  double res;
  double value,p,pt,pf,BChild0,BChild1,e0,e1;
  double * value_p,** eta_rule;
  DdNode *nodekey,*T,*F;

  comp=Cudd_IsComplement(node);
  comp=(comp && !comp_par) ||(!comp && comp_par);
  if (Cudd_IsConstant(node))
  {
    value=Cudd_V(node);
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
    value_p=get_value(nodesB,nodekey);
    if (value_p!=NULL)
    {
      return *value_p;
    }
    else
    {
      index=Cudd_NodeReadIndex(node);
      p=probs_ex[nex][index];
      T = Cudd_T(node);
      F = Cudd_E(node);
      pf=ProbPath(F,comp,nex);
      pt=ProbPath(T,comp,nex);
      BChild0=pf*(1-p);
      BChild1=pt*p;
      value_p=get_value(nodesF,nodekey);
      e0 = (*value_p)*BChild0; 
      e1 = (*value_p)*BChild1; 
      mVarIndex=bVar2mVar_ex[nex][index];
      v=vars_ex[nex][mVarIndex];
      pos=index-v.firstBoolVar;
      eta_rule=eta_temp[v.nRule];
      eta_rule[pos][0]=eta_rule[pos][0]+e0;
      eta_rule[pos][1]=eta_rule[pos][1]+e1;
      res=BChild0+BChild1;
      add_node(nodesB,nodekey,res);
      position=Cudd_ReadPerm(mgr_ex[nex],index);
      position=position+1;
      boolVarIndex=Cudd_ReadInvPerm(mgr_ex[nex],position);//Returns the index of the variable currently in the i-th position of the order. 
      if (position<boolVars_ex[nex])
      {
        sigma[position]=sigma[position]+e0+e1;
      }
      if(!Cudd_IsConstant(T))
      {
        index=Cudd_NodeReadIndex(T);  
        position=Cudd_ReadPerm(mgr_ex[nex],index);
        sigma[position]=sigma[position]-e1;
      }
      
      if(!Cudd_IsConstant(F))
      {
        index=Cudd_NodeReadIndex(F);
        position=Cudd_ReadPerm(mgr_ex[nex],index);
        sigma[position]=sigma[position]-e0;
      }
    
      return res;
    }
  }
}




void Forward(DdNode *root, int nex)
{
  int i,j;

  if (boolVars_ex[nex])
  {
    nodesToVisit= (DdNode ***)malloc(sizeof(DdNode **)* boolVars_ex[nex]);
    NnodesToVisit= (int *)malloc(sizeof(int)* boolVars_ex[nex]);
    nodesToVisit[0]=(DdNode **)malloc(sizeof(DdNode *));
    nodesToVisit[0][0]=root;
    NnodesToVisit[0]=1;
    for(i=1;i<boolVars_ex[nex];i++)
    {
      nodesToVisit[i]=NULL;
      NnodesToVisit[i]=0;
    }
    add_node(nodesF,Cudd_Regular(root),1);
    for(i=0;i<boolVars_ex[nex];i++)
    {
      for(j=0;j<NnodesToVisit[i];j++)
      UpdateForward(nodesToVisit[i][j],nex);
    }
    for(i=0;i<boolVars_ex[nex];i++)
    {
      free(nodesToVisit[i]);
    }
    free(nodesToVisit);
    free(NnodesToVisit);
  }
  else
  {
    add_node(nodesF,Cudd_Regular(root),1);
  }
}

void UpdateForward(DdNode *node, int nex)
{
  int index,position,mVarIndex;
  DdNode *T,*E,*nodereg;
  variable v;
  double *value_p,*value_p_T,*value_p_F,p;

  if (Cudd_IsConstant(node)) 
  {
    return;
  }
  else
  {
    index=Cudd_NodeReadIndex(node);
    mVarIndex=bVar2mVar_ex[nex][index];
    v=vars_ex[nex][mVarIndex];
    p=probs_ex[nex][index];
    nodereg=Cudd_Regular(node);
    value_p=get_value(nodesF,nodereg);
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
        value_p_T=get_value(nodesF,T);
        if (value_p_T!= NULL)
        {
           *value_p_T= *value_p_T+*value_p*p;
        }
        else
        {
          add_or_replace_node(nodesF,Cudd_Regular(T),*value_p*p);
          index=Cudd_NodeReadIndex(T);
          position=Cudd_ReadPerm(mgr_ex[nex],index);
          nodesToVisit[position]=(DdNode **)realloc(nodesToVisit[position],
	    (NnodesToVisit[position]+1)* sizeof(DdNode *));
          nodesToVisit[position][NnodesToVisit[position]]=T;
          NnodesToVisit[position]=NnodesToVisit[position]+1;
        }
      }
      if (!Cudd_IsConstant(E)) 
      {
        value_p_F=get_value(nodesF,Cudd_Regular(E));

        if (value_p_F!= NULL)
        {
          *value_p_F= *value_p_F+*value_p*(1-p);
        }
        else
        {
          add_or_replace_node(nodesF,Cudd_Regular(E),*value_p*(1-p));
          index=Cudd_NodeReadIndex(E);
          position=Cudd_ReadPerm(mgr_ex[nex],index);
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


int indexMvar(DdNode * node)
{
  int index,mVarIndex;

  index=Cudd_NodeReadIndex(node);
  mVarIndex=bVar2mVar_ex[ex][index];
  return mVarIndex;
}



double GetOutsideExpe(DdNode *root,double ex_prob, int nex)
{
  int i,j,mVarIndex,bVarIndex;
  double **eta_rule;
  double theta,rootProb, T=0;


  sigma=(double *)malloc(boolVars_ex[nex] * sizeof(double));

  for (j=0; j<boolVars_ex[nex]; j++)
  {
    sigma[j]=0;
  }
  for (j=0; j<nRules; j++)
  {
    for (i=0; i<rules[j]-1; i++)
    {
      eta_temp[j][i][0]=0;
      eta_temp[j][i][1]=0;
    }
  }
  rootProb=ProbPath(root,0,nex);
  if (rootProb>0.0)
  {
    for (j=0; j<boolVars_ex[nex]; j++)
    {
      T += sigma[j];
      bVarIndex=Cudd_ReadInvPerm(mgr_ex[nex],j);
      if (bVarIndex==-1)  
      {
        bVarIndex=j;
      }

      mVarIndex=bVar2mVar_ex[nex][bVarIndex];
      eta_rule=eta_temp[vars_ex[nex][mVarIndex].nRule];
      for (i=0; i<vars_ex[nex][mVarIndex].nVal-1;i++)
      {
        theta=probs_ex[nex][bVarIndex];
        eta_rule[i][0]=eta_rule[i][0]+T*(1-theta);
        eta_rule[i][1]=eta_rule[i][1]+T*theta;
      }   
    }

    for (j=0; j<nRules; j++)
    {
      for (i=0; i<rules[j]-1; i++)
      {
        eta[j][i][0]=eta[j][i][0]+eta_temp[j][i][0]*ex_prob/rootProb;
        eta[j][i][1]=eta[j][i][1]+eta_temp[j][i][1]*ex_prob/rootProb;
      }
    }
  }
  free(sigma);
  return rootProb;
}


void Maximization(void)
{
  int r,i,j,e;
  double sum=0;
  double *probs_rule,**eta_rule;

  for (r=0;r<nRules;r++)
  {
    eta_rule=eta[r];
    for (i=0;i<rules[r]-1;i++)
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

  for(e=0;e<ex;e++)
  {
    for (j=0;j<nVars_ex[e];j++)
    {
      r=vars_ex[e][j].nRule;
      probs_rule=arrayprob[r];
      for(i=0;i<rules[r]-1;i++)
      {    
        probs_ex[e][vars_ex[e][j].firstBoolVar+i]=probs_rule[i];
      }
    }
  }
}

static int randomize(void)
{
  int i,j,e,rule;
  double * theta,p0;
  double pmass,par;
  double **Theta_rules;
  
  Theta_rules=(double **)malloc(nRules *sizeof(double *));

  for (j=0;j<nRules;j++)
  {
    Theta_rules[j]=(double *)malloc(rules[j]*sizeof(double));
  }

  for (j=0;j<nRules;j++)
  {
    theta=Theta_rules[j];
    pmass=0;
    for (i=0;i<rules[j]-1;i++)
    {
      par=((double)rand())/RAND_MAX*(1-pmass);
      pmass=pmass+par;
      theta[i]=par;
    }
    theta[rules[j]-1]=1-pmass;
  }
  for(e=0;e<ex;e++)
  {
    for (j=0; j<nVars_ex[e]; j++)
    {
      rule=vars_ex[e][j].nRule;
      theta=Theta_rules[rule];
      p0=1;
      for (i=0; i<vars_ex[e][j].nVal-1;i++)
      {    
        probs_ex[e][vars_ex[e][j].firstBoolVar+i]=theta[i]/p0;
        p0=p0*(1-theta[i]/p0);
      }
    }
  }
  for (j=0;j<nRules;j++)
  {
    free(Theta_rules[j]);
  }
  free(Theta_rules);
  return 1;
}

static int EM(void)
{
  YAP_Term arg1,arg2,arg3,arg4,arg5,arg6,arg7,arg8,
    out1,out2,out3,nodesTerm,ruleTerm,tail,pair,compoundTerm;
  DdNode * node1,**nodes_ex;
  int r,lenNodes,i,iter;
  long iter1;
  double CLL0= -2.2*pow(10,10); //-inf
  double CLL1= -1.7*pow(10,8);  //+inf   
  double p,p0,**eta_rule,ea,er; 
  double ratio,diff;

  arg1=YAP_ARG1;
  arg2=YAP_ARG2;
  arg3=YAP_ARG3;
  arg4=YAP_ARG4;
  arg5=YAP_ARG5;
  arg6=YAP_ARG6;
  arg7=YAP_ARG7;
  arg8=YAP_ARG8;

  nodesTerm=arg1; 
  ea=YAP_FloatOfTerm(arg2);
  er=YAP_FloatOfTerm(arg3);
  lenNodes=YAP_IntOfTerm(arg4);  
  iter=YAP_IntOfTerm(arg5);  

  nodes_ex=(DdNode **)malloc(lenNodes*sizeof(DdNode*));
  nodes_probs_ex=(double *)malloc(lenNodes*sizeof(double));
  example_prob=(double *)malloc(lenNodes*sizeof(double));

  for (i=0;i<lenNodes;i++)
  {
    pair=YAP_HeadOfTerm(nodesTerm);
    node1=(DdNode *)YAP_IntOfTerm(YAP_HeadOfTerm(pair));
    nodes_ex[i]=node1;
    pair=YAP_TailOfTerm(pair);
    example_prob[i]=YAP_FloatOfTerm(YAP_HeadOfTerm(pair));
    nodesTerm=YAP_TailOfTerm(nodesTerm);
  }
  diff=CLL1-CLL0;
  ratio=diff/fabs(CLL0);
  if (iter==-1)
    iter1= 2147000000;
  else iter1=iter;
  

  while  ( (diff>ea) && (ratio>er) && (cycle<iter1) )
  {
    cycle++;
    for (r=0;r<nRules;r++)
    {
      for (i=0;i<rules[r]-1;i++)
      {
        eta_rule=eta[r];
        eta_rule[i][0]=0;
        eta_rule[i][1]=0;
      }
    }
    CLL0 = CLL1;
    CLL1 = Expectation(nodes_ex,lenNodes);
    Maximization();
    diff=CLL1-CLL0;
    ratio=diff/fabs(CLL0);
  }
  out2= YAP_TermNil();
  for (r=0; r<nRules; r++)
  {
    tail=YAP_TermNil();
    p0=1;
    for (i=0;i<rules[r]-1;i++)
    {
      p=arrayprob[r][i]*p0;
      tail=YAP_MkPairTerm(YAP_MkFloatTerm(p),tail);
      p0=p0*(1-arrayprob[r][i]);
    }
    tail=YAP_MkPairTerm(YAP_MkFloatTerm(p0),tail);
    ruleTerm=YAP_MkIntTerm(r);
    compoundTerm=YAP_MkPairTerm(ruleTerm,YAP_MkPairTerm(tail,YAP_TermNil()));
    out2=YAP_MkPairTerm(compoundTerm,out2);
  }
  out3= YAP_TermNil();
  for (i=0;i<lenNodes;i++)
  {
    out3=YAP_MkPairTerm(YAP_MkFloatTerm(nodes_probs_ex[i]),out3);
  }
  YAP_Unify(out3,arg8);

  out1=YAP_MkFloatTerm(CLL1);
  YAP_Unify(out1,arg6);
  free(nodes_ex);
  free(example_prob);
  free(nodes_probs_ex);

  return (YAP_Unify(out2,arg7));
}


static int Q(void)
{
  YAP_Term arg1,arg2,arg3,arg4,out,out1,
    term,nodesTerm,ruleTerm,tail,pair,compoundTerm;
  DdNode * node1,**nodes_ex;
  int r,lenNodes,i;
  double p1,p0,**eta_rule,CLL; 

  arg1=YAP_ARG1;
  arg2=YAP_ARG2;
  arg3=YAP_ARG3;
  arg4=YAP_ARG4;

  nodesTerm=arg1; 
  lenNodes=YAP_IntOfTerm(arg2);  

  nodes_ex=(DdNode **)malloc(lenNodes*sizeof(DdNode*));
  example_prob=(double *)malloc(lenNodes*sizeof(double));

  for (i=0;i<lenNodes;i++)
  {
    pair=YAP_HeadOfTerm(nodesTerm);
    node1=(DdNode *)YAP_IntOfTerm(YAP_HeadOfTerm(pair));
    nodes_ex[i]=node1;
    pair=YAP_TailOfTerm(pair);
    example_prob[i]=YAP_FloatOfTerm(YAP_HeadOfTerm(pair));
    nodesTerm=YAP_TailOfTerm(nodesTerm);
  }

  for (r=0;r<nRules;r++)
  {
    for (i=0;i<rules[r]-1;i++)  
    {
      eta_rule=eta[r];
      eta_rule[i][0]=0;
      eta_rule[i][1]=0;
    }
  }
  CLL=Expectation(nodes_ex,lenNodes);
  out= YAP_TermNil();

  for (r=0; r<nRules; r++)
  {
    tail=YAP_TermNil();
    eta_rule=eta[r];
    for (i=0;i<rules[r]-1;i++)
    {
      p0=eta_rule[i][0];
      p1=eta_rule[i][1];
      term=YAP_MkPairTerm(YAP_MkFloatTerm(p0),
        YAP_MkPairTerm(YAP_MkFloatTerm(p1),YAP_TermNil()));
      tail=YAP_MkPairTerm(term,tail);
    }

    ruleTerm=YAP_MkIntTerm(r);
    compoundTerm=YAP_MkPairTerm(ruleTerm,YAP_MkPairTerm(tail,YAP_TermNil()));
    out=YAP_MkPairTerm(compoundTerm,out);
  }

  free(nodes_ex);
  free(example_prob);

  out1=YAP_MkFloatTerm(CLL);
  YAP_Unify(out1,arg4);
  return (YAP_Unify(out,arg3));
}

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

void init_my_predicates()
/* function required by YAP for intitializing the predicates defined by a C function*/
{
  srand(10);

  YAP_UserCPredicate("init",init,2);
  YAP_UserCPredicate("init_bdd",init_bdd,0);
  YAP_UserCPredicate("end",end,0);
  YAP_UserCPredicate("end_bdd",end_bdd,0);
  YAP_UserCPredicate("add_var",add_var,4);
  YAP_UserCPredicate("equality",equality,3);
  YAP_UserCPredicate("and",and,3);
  YAP_UserCPredicate("one",one,1);
  YAP_UserCPredicate("zero",zero,1);
  YAP_UserCPredicate("or",or,3);
  YAP_UserCPredicate("bdd_not",bdd_not,2);
  YAP_UserCPredicate("create_dot",create_dot,2);
  YAP_UserCPredicate("init_test",init_test,1);
  YAP_UserCPredicate("end_test",end_test,0);
  YAP_UserCPredicate("ret_prob",ret_prob,2);
  YAP_UserCPredicate("em",EM,8);
  YAP_UserCPredicate("q",Q,4);
  YAP_UserCPredicate("randomize",randomize,0);
  YAP_UserCPredicate("deref",rec_deref,1);
  YAP_UserCPredicate("garbage_collect",garbage_collect,2);
  YAP_UserCPredicate("bdd_to_add",bdd_to_add,2);
  YAP_UserCPredicate("paths_to_non_zero",paths_to_non_zero,2);
  YAP_UserCPredicate("paths",paths,2);
  YAP_UserCPredicate("dag_size",dag_size,2);
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
