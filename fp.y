%{
#include <stdio.h>
#include <string.h>
void yyerror(const char *msg);
typedef struct NODE
{
    int num;            /* its number, if any (e.g. a number node) */
    char type;          /* the type will determine which operation will be operated when traversing AST tree */
    char* id;           /* its id, if any (e.g. an ID node) */
    int INSIDEFUN;      /* if this node is created when defining a function (ex. a variable node in function), then this value will be 1. Otherwise it'll be 0. */
    int sign;           /* When traversing the function RULE node, this sign value will be 1 when id nodes being changed to number nodes. */
                        /* After getting the result, this value will go back to 0 (so the node wont conflict with other operations). */
    struct NODE *left;
    struct NODE *right;
    struct NODE *third; /* for if and function statements */
}
*Node;

typedef struct FUNCTIONNODE
{
    struct NODE *RULE;          /* the rule of the function. it should be an Operation Node (+ - * / & < ...)*/
    struct NODE *PARAMS;        /* the PARAMS of the function. it should be an 'S' node (IDs). */
    char* id;                   /* the name of the function. literally. */
}
*functionNode;

int SYNTAXERROR = 0; /* will turn up when syntax error detected */
int ans = 0;
Node Origin = NULL; /* root */
Node temp_Node = NULL;
Node temp2_Node = NULL;
Node temp3_Node = NULL;
functionNode tempFunc_Node = NULL;
functionNode tempFunc2_Node = NULL;
Node var[20];                   /* The array where I store all the global variable nodes in it. */
functionNode func[20];          /* The array where I store all the function nodes in it. */
int varIdx = 0;                 
int funcIdx = 0;
int temp_Int, temp2_Int = 0;

Node makeNode(Node, Node, char);
void Revert(Node); //only used in function
functionNode makeFuncNode(Node, Node, char*);
void PreOrder(Node);
void Run(Node);
void FunBody(Node, Node);
%}
%union{
    int num;
    char* word;
    char ch;
    struct NODE* node;
}

%type<node> STMT STMTS EXP PRINT-STMT IF-EXP TEST-EXP THEN-EXP ELSE-EXP PRINT-BOOL PRINT-NUM
%type<node> NUM-OP
%type<node> LOGICAL-OP
%type<node> PLUS PLUS_MORE MINUS MULTIPLY MULTIPLY_MORE DIVIDE MODULUS GREATER SMALLER EQUAL
%type<node> AND-OP AND_MORE OR-OP OR_MORE NOT-OP
%type<node> DEF-STMT VARIABLE FUN-EXP FUN-IDs FUN-BODY FUN-NAME PARAM PARAMS IDS FUN-CALL

%token<num> bool-val number
%token<ch> lp rp plus minus mul division mod larger smaller equal true false and or not If define
%token<word> printnum printbool id fun

%%
PROGRAM		: STMTS 
            {
                /* 這樣root就會是最後一個被建起來的Node (大概吧) */
                Origin = $1;
                //PreOrder(Origin);
                //printf("yep\n");
            }
STMTS       : STMT STMTS 
            {
                /* make a Node to connect STMT and STMTS */
                /* left = $1 (STMT), right = $2 (STMTS), type = 'B' */
                Node n = makeNode($1, $2, 'B');
                $$ = n;    
            }
            | STMT
            {
                $$ = $1;
            }
STMT		: EXP | PRINT-STMT | DEF-STMT
PRINT-STMT	: PRINT-NUM | PRINT-BOOL
            ;
            
PRINT-NUM   : lp printnum EXP rp
            {
                /* make a node for printnum */
                /* left = $3 (EXP), right = NULL, type = 'p' */
                Node n = makeNode($3, NULL, 'p');
                $$ = n;
            } 
            ;
PRINT-BOOL  : lp printbool EXP rp
            {
                /* make a node for printbool */
                /* left = $3 (EXP), right = NULL, type = 'P' */
                Node n = makeNode($3, NULL, 'P');
                $$ = n;
            }
EXP		    : true 
            {
                /* make a node for numbers*/
                /* left = NULL, right = NULL, type = 't' */
                Node n = makeNode(NULL, NULL, 't');
                n->num = $1;
                $$ = n;
            }
            | false 
            {
                /* make a node for numbers*/
                /* left = NULL, right = NULL, type = 't' */
                Node n = makeNode(NULL, NULL, 't');
                n->num = $1;
                $$ = n;
            }
            | number
            {
                /* make a node for numbers*/
                /* left = NULL, right = NULL, type = 'n' */
                Node n = makeNode(NULL, NULL, 'n');
                n->num = $1;
                $$ = n;
            }
            | NUM-OP | LOGICAL-OP | IF-EXP | VARIABLE | FUN-EXP | FUN-CALL 
            ;

NUM-OP		: PLUS | MINUS | MULTIPLY | DIVIDE | MODULUS | GREATER | SMALLER | EQUAL
            ;
PLUS		: lp plus EXP PLUS_MORE rp 
            {
                /* make a node for PLUS */
                /* left = $3, right = $4, type = '+' */
               Node n = makeNode($3, $4, '+');
               //printf("$4ㄉtype= %c\n", $4->type);
               $$ = n;
               //printf("$$ㄉtype= %c\n", $$->type);
               //printf("有來+\n");
            }
            ;
PLUS_MORE   : EXP PLUS_MORE 
            {
                Node n = makeNode($1, $2, '+');
               $$ = n;
            }
            | EXP {$$ = $1;}
            ;

MINUS		: lp minus EXP EXP rp
            {
                /* make a node for MINUS */
                /* left = $3, right = $4, type = '-' */
               Node n = makeNode($3, $4, '-');
               $$ = n;
            }
            ;

MULTIPLY	: lp mul EXP MULTIPLY_MORE rp
            {
                /* make a node for MULTIPLY */
                /* left = $3, right = $4, type = '*' */
               Node n = makeNode($3, $4, '*');
               $$ = n;
            }
            ;

MULTIPLY_MORE : EXP MULTIPLY_MORE
            {
                Node n = makeNode($1, $2, '*');
                $$ = n;
            }
            | EXP {$$ = $1;}
            ;

DIVIDE		: lp division EXP EXP rp
            {
                /* make a node for DIVIDE */
                /* left = $3, right = $4, type = '/' */
               Node n = makeNode($3, $4, '/');
               $$ = n;
            }
            ;

MODULUS		: lp mod EXP EXP rp
            {
                /* make a node for MODULUS */
                /* left = $3, right = $4, type = '%' */
               Node n = makeNode($3, $4, '%');
               $$ = n;
            }
GREATER		: lp larger EXP EXP rp
            {
                /* make a node for GREATER */
                /* left = $3, right = $4, type = '>' */
               Node n = makeNode($3, $4, '>');
               $$ = n;
            }
SMALLER		: lp smaller EXP EXP rp
            {
                /* make a node for SMALLER */
                /* left = $3, right = $4, type = '<' */
               Node n = makeNode($3, $4, '<');
               $$ = n;
            }
EQUAL		: lp equal EXP EXP rp
            {
                /* make a node for EQUAL */
                /* left = $3, right = $4, type = '=' */
               Node n = makeNode($3, $4, '=');
               $$ = n;
            }
LOGICAL-OP	: AND-OP | OR-OP | NOT-OP
AND-OP		: lp and EXP AND_MORE rp
            {
                /* make a node for AND-OP */
                /* left = $3, right = $4, type = '&' */
               Node n = makeNode($3, $4, '&');
               $$ = n;
            }
            ;
AND_MORE : EXP AND_MORE
            {
               Node n = makeNode($1, $2, '&');
               $$ = n;
            }
            | EXP {$$ = $1;}
            ;
OR-OP		: lp or EXP OR_MORE rp
            {
                /* make a node for OR-OP */
                /* left = $3, right = $4, type = '|' */
               Node n = makeNode($3, $4, '|');
               $$ = n;
            }
            ;
OR_MORE     : EXP OR_MORE
            {
                Node n = makeNode($1, $2, '|');
                $$ = n;
            }
            | EXP {$$ = $1;}
            ;
NOT-OP		: lp not EXP rp
            {
                /* make a node for AND-OP */
                /* left = $3, right = NULL, type = '!' */
               Node n = makeNode($3, NULL, '!');
               $$ = n;
            }
            ;
IF-EXP		: lp If TEST-EXP THEN-EXP ELSE-EXP rp
            {
                /* make a node for IF */
                /* left = $4, right = #5, type = 'I' */
                /* There will be a third pointer inside the IF nodes, as there are 3 parameters in this rule*/
                /* aka third = $3 */
               Node n = makeNode($4, $5, 'I');
               n->third = $3;
               $$ = n;
            }
            ;
TEST-EXP	: EXP
            {
                $$ = $1;
            }
            ;
THEN-EXP	: EXP
            {
                $$ = $1;
            }
            ;
ELSE-EXP	: EXP
            {
                $$ = $1;
            }
            ;

DEF-STMT	: lp define VARIABLE EXP rp
            {
                /* make a node for DEFINE */
                /* left = $3, right = $4, type = 'd' */
               Node n = makeNode($3, $4, 'd');
               $$ = n;
            }
            | lp define VARIABLE FUN-EXP rp
            {
                $3->INSIDEFUN = 1; // To indicate if the param is inside the function or not
                Node n = makeNode($3, $4, 'D');
                $$ = n;
            }
            ;

VARIABLE    : id
            {
                Node n = makeNode(NULL, NULL, 'i');
                n->id = $1;
                $$ = n;
            }
            ;
FUN-EXP	    : lp fun FUN-IDs FUN-BODY rp 
            {
                /* FUN-IDs = x y, n->left = x, x->left = y */
                /* FUN-BODY = x + y, n->left = x, n->right = y */

                /* make a node for FUNCTION */
                /* left = $3, right = $4, type = 'f' */

                Node n = makeNode($3, $4, 'f');
                $$ = n;
            }
            ;
FUN-IDs	    : lp IDS rp
            {
                Node n = makeNode($2, NULL, 'S');
                $$ = n;
            }
            | lp rp /* no args */
            {
                //printf("lp rp no args\n");
                Node n = makeNode(NULL, NULL, 'S');
                $$ = n;
            }
            ;
IDS         : VARIABLE IDS
            {
                $1->left = $2;      /* 不想讓 id 是分兩邊的，雖然不知道這樣會怎樣 */
                $$ = $1;
            }
            | VARIABLE 
            {
                $$ = $1;
            }
            ;
FUN-BODY	: EXP
            {
                $$ = $1;
            }
            ;
FUN-CALL	: lp FUN-EXP PARAMS rp  /* 沒定義名字的函數 */
            {
                /* make a node for FUNCALL */
                /* left = $2, right = $3, type = 'F' */
                Node n = makeNode($2, $3, 'F');
                $$ = n;   
            }
            | lp FUN-NAME PARAMS rp  /* 有定義 */
            {
                /* make a node for DEFINEDFUNCALL */
                /* left = $2, right = $3, type = 'c' */
                {
                    Node n = makeNode($2, $3, 'c');
                    $$ = n;
                }
            }
            ;
PARAMS      : PARAM
            {
                Node n = makeNode($1, NULL, 's');
                $$ = n;
            }
            |
            {
                Node n = makeNode(NULL, NULL, 's');
                $$ = n;
            }
PARAM		: EXP PARAM            /* 可能有多個PARAM */
            {
                /* 他都在左邊 */
                $1->left = $2;
                $$ = $1;
            }
            | EXP
            {
                $$ = $1;
            }            
            ;
FUN-NAME	: VARIABLE
            ;



%% 

void yyerror(const char *msg)
{
    fputs(msg, stderr);
    printf("\n");
    SYNTAXERROR = 1;
}

void Run(Node Root)
{
    //printf("%c\n", Root->type);
    if (Root == NULL) return;

    if (Root->type == 'B') /* STMT -> STMTS */
    {
        Run(Root->left);
        Run(Root->right);
    }
    else if (Root->type == 'p') /* printNUM */
    {
        Run(Root->left);
        printf("%d\n", Root->left->num);
    }
    else if (Root->type == 'P') /* printBOOL */
    {
        Run(Root->left);
        if (Root->left->num == 1) printf("#t\n");
        else printf("#f\n");
    }
    else if (Root->type == '+') /* add */ 
    {
        Run(Root->left);
        Run(Root->right);

        if (Root->left && Root->right) Root->num = Root->left->num + Root->right->num;

        //printf("(+)Root->num = %d\n", Root->num);
    }
    else if (Root->type == '-') /* minus */
    {
        Run(Root->left);
        Run(Root->right);

        if (Root->left && Root->right) Root->num = Root->left->num - Root->right->num;
        //printf("(-)Root->num = %d\n", Root->num);   
    }
    else if (Root->type == '*') /* multiply */
    {
        Run(Root->left);
        Run(Root->right);

        if (Root->left && Root->right) Root->num = Root->left->num * Root->right->num;
        //printf("(*)Root->num = %d\n", Root->num);
    }
    else if (Root->type == '/') /* divide */
    {
        Run(Root->left);
        Run(Root->right);

        if (Root->left && Root->right) Root->num = Root->left->num / Root->right->num;
        //printf("(/)Root->num = %d\n", Root->num);
    }
    else if (Root->type == '%') /* modulus */
    {
        Run(Root->left);
        Run(Root->right);

        if (Root->left && Root->right) Root->num = Root->left->num % Root->right->num;
        //printf("(%)Root->num = %d\n", Root->num);
    }
    else if (Root->type == '>') /* Greater */
    {
        Run(Root->left);
        Run(Root->right);

        if (Root->left && Root->right) 
        {
            if (Root->left->num > Root->right->num) Root->num = 1;
            else Root->num = 0;
        }
        //printf("(>)Root->num = %d\n", Root->num);
    }
    else if (Root->type == '&') /* And */
    {
        Run(Root->left);
        Run(Root->right);

        if (Root->left && Root->right) { Root->num = Root->left->num & Root->right->num; };
        
        //if (Root->num == 1) printf("(&)Root->num = #t\n");
        //else printf("(&)Root->num = #f\n");
    }
    else if (Root->type == '|') /* or */
    {
        Run(Root->left);
        Run(Root->right);

        if (Root->left && Root->right) { Root->num = Root->left->num | Root->right->num; };
        
        //if (Root->num == 1) printf("(|)Root->num = #t\n");
        //else printf("(|)Root->num = #f\n");
    }
    else if (Root->type == '!') /* not */
    {
        Run(Root->left);

        if (Root->left) { Root->num = (Root->left->num == 1)? 0 : 1;}
        
        // if (Root->num == 1) printf("(!)Root->num = #t\n");
        // else printf("(!)Root->num = #f\n");
    }
    else if (Root->type == '>') /* GREATER */
    {
        Run(Root->left);
        Run(Root->right);

        if (Root->left && Root->right) { Root->num = (Root->left->num > Root->right->num)? 1 : 0; };
        
        // if (Root->num == 1) printf("(>)Root->num = #t\n");
        // else printf("(>)Root->num = #f\n");
    }
    else if (Root->type == '<') /* SMALLER */
    {
        Run(Root->left);
        Run(Root->right);

        if (Root->left && Root->right) { Root->num = (Root->left->num < Root->right->num)? 1 : 0; };
        
        // if (Root->num == 1) printf("(<)Root->num = #t\n");
        // else printf("(<)Root->num = #f\n");
    }
    else if (Root->type == '=') /* EQUAL */
    {
        Run(Root->left);
        Run(Root->right);

        if (Root->left && Root->right) { Root->num = (Root->left->num == Root->right->num)? 1 : 0; };
        
        // if (Root->num == 1) printf("(=)Root->num = #t\n");
        // else printf("(=)Root->num = #f\n");
    }
    else if (Root->type == 'I') /* IF */
    {
        /* left = nodes for TRUE, right = nodes for FALSE, third = 反正就判斷的那個node*/
        Run(Root->left);
        Run(Root->right);
        Run(Root->third);

        if (Root->third)
        {
            if (Root->third->num == 1) Root->num = Root->left->num; /* true */
            else Root->num = Root->right->num; /* false */
        }

        // if (Root->num == 1) printf("(IF)Root->num = #t\n");
        // else printf("(IF)Root->num = #f\n");
    }
    else if (Root->type == 'd') /* DEFINE */
    {
        Run(Root->left);
        Run(Root->right);

        if (Root->left && Root->right)
        {
            Node n = makeNode(NULL, NULL, 'v');
            n->id = Root->left->id;
            n->num = Root->right->num;
            var[varIdx] = n;
            varIdx++;
        }
    }
    else if (Root->type == 'i') /* ID */
    {
        if (Root->INSIDEFUN == 1) return;
       for (temp_Int = 0; temp_Int < varIdx; temp_Int++)
       {
           if (strncmp(var[temp_Int]->id, Root->id, strlen(Root->id)) == 0) /* compare id */
           {
               Root->num = var[temp_Int]->num;
               break;
           }
       }
    }
    else if (Root->type == 'n')
    {
        if (Root->left) Run(Root->left);
        if (Root->right) Run(Root->right);
    }
    else if (Root->type == 'F') /* undefined function */
    {
        /*        F         */
        /*     /     \      */
        /*   f         S    */
        /*  / \       /     */
        /*ID  body  Param1  */
        /*            /     */
        /*        Param2    */

        Run(Root->left);
        Run(Root->right);
        
        temp_Node = Root->left->left->left;  //第一個ID
        temp2_Node = Root->right->left;      //第一個PARAM
        while(1)
        {
            /* bind IDs with PARAMs */
            temp_Node->num = temp2_Node->num;
            if (temp_Node->left) 
            {
                temp_Node = temp_Node->left;
                temp2_Node = temp2_Node->left; // 希望不要爆拜託
            }
            else break;
        }
        FunBody(Root->left->left, Root->left->right);
        // PreOrder(Root->left->right);
        Run(Root->left->right); 

        Root->num = Root->left->right->num;
    }
    else if (Root->type == 'S') /* IDs */
    {
        if (Root->left) Run(Root->left);
    }
    else if (Root->type == 'f') /* FUN-EXP (Fun-IDs & FUN-Body) */
    {
        Run(Root->left); 
        /* Run(right_ptr); 這裡好像有點危 */
    }
    else if (Root->type == 's') /* PARAMS */
    {
        if (Root->left) Run(Root->left);
    }
    else if (Root->type == 'D') /* Defined (Named) Function*/
    {
        /*        D             */
        /*     /     \          */
        /*   id        f        */
        /*           /   \      */
        /*         IDs    BODY  */
        /*                      */
        /*                      */
        Run(Root->left);
        Run(Root->right);

        /* Store the function node into the function array (func[])*/

        func[funcIdx] = makeFuncNode(Root->right->right, Root->right->left, Root->left->id);
        funcIdx++;
        // printf("有活到這裡: D END\n");
    }
    else if (Root->type == 'c') /* Defined Function Call */
    {
        /*        c             */
        /*     /     \          */
        /*   id        S        */
        /*           /          */
        /*         Param1       */
        /*         /            */
        /*      Param2          */

        /* Find the function node inside the array that has the same name as Root->left (id) */

        Run(Root->left);
        Run(Root->right);

        
        //printf("有活到這裡: c 0.5/3\n");
        for (temp_Int = 0; temp_Int < funcIdx; temp_Int++)
        {
            if (strncmp(Root->left->id, func[temp_Int]->id, strlen(Root->left->id)) == 0)
            {
                tempFunc_Node = func[temp_Int];
                // printf("tempFunc_Node->name = %s\n", tempFunc_Node->id);
                // printf("tempFunc_Node->PARAMS :\n");
                // PreOrder(tempFunc_Node->PARAMS);
                // printf("tempFunc_Node->RULE :\n");
                // PreOrder(tempFunc_Node->RULE);
                temp_Node = tempFunc_Node->PARAMS; 
                temp3_Node = tempFunc_Node->RULE;

                break;
            }
        }
        
        /* There should be a warning/error for unable to find the function, but for now I'll just skip that lols. */
        
        /* Bind the PARAMS in the function node with Param first */
        /* tempNode = func[temp_Int]->PARAMS (ids), temp_Node2 = Root->right (numbers) */
       
        // printf("有活到這裡: c 1/3\n");
        temp2_Node = Root->right; 

        // 防止沒有Param的function出現
        if (temp_Node->left != NULL | temp2_Node->left != NULL)
        {
            temp_Node = temp_Node->left;    // id的第一個
            temp2_Node = temp2_Node->left;  // PARAMS的第一個
            while(1)
            {
                /* bind IDs with PARAMs */
                temp_Node->num = temp2_Node->num;
                if (temp_Node->left) 
                {
                    temp_Node = temp_Node->left;
                    temp2_Node = temp2_Node->left; // 這裡可能會有temp2_node沒有left這件事... 但管他ㄉ...
                }
                else break;
            }
        }
        // printf("有活到這裡: c 2/3\n");

        /* Replace all the params in the function node with Param1, Param2... */
        /* temp_Node = Binded Params and numbers, temp_Node3 = Rules */

        temp_Node = tempFunc_Node->PARAMS;
        FunBody(temp_Node, temp3_Node);
        //PreOrder(temp3_Node); // debug
        Run(temp3_Node);
        temp3_Node = tempFunc_Node->RULE; // 為了不要動到function內的內容，都把東西搬到temp去做操作
       
        /* 先看一下temp_Node和temp3_Node長怎樣*/

        // printf("\n看一下temp_Node長怎樣\n");
        // PreOrder(temp_Node);
        // printf("看一下temp3_Node\n");
        // PreOrder(temp3_Node);
        // printf("\n");
        /* Return the value back to the top Nodes for the sake of print-funcs */
        Root->num = tempFunc_Node->RULE->num; 
        // printf("看一下結束後的function array裡面Node長怎樣\n");
        // PreOrder(func[temp_Int]->RULE);
        // printf("\n");
        
        Revert(func[temp_Int]->RULE); /* 這裡要改 */

        // printf("看一下復原後的function array裡面Node長怎樣\n");
        // PreOrder(func[temp_Int]->RULE);
        // printf("\n");

        tempFunc_Node = NULL;
        temp_Node = NULL;
        temp2_Node = NULL;
        temp3_Node = NULL;
        // printf("有活到這裡: c END\n"); 
    }
    
}


Node makeNode(Node left_ptr, Node right_ptr, char t)
{
    Node n = (Node)(malloc(sizeof(struct NODE)));

    n->type = t;
    n->left = left_ptr;
    n->right = right_ptr;
    n->sign = 0;
    
    return n;
}

void Revert(Node node)
{
    if (node->sign == 1)
    {
        node->num = 0;
        node->sign = 0;
        node->type = 'i';
    }

    if (node->left) Revert(node->left);
    if (node->right) Revert(node->right);
}
void FunBody(Node IDs, Node Rules)
{
    /* Pass進來的應該要是'S'和'+ - * /' */
    // printf("IDs->type = %c\n", IDs->type);
    // printf("Rules->type = %c\n", Rules->type);
    /* traverse Rules and replace all id nodes to num nodes */
    if (Rules->type == 'i')
    {
        temp2_Node = IDs;
        while(temp2_Node->left != NULL)
        {
            // printf("temp2_Node->left->id = %s\n", temp2_Node->left->id);
            // printf("Rules->id = %s\n", Rules->id);
            if (strncmp(temp2_Node->left->id, Rules->id, strlen(Rules->id)) == 0)
            {
                
                Rules->num = temp2_Node->left->num;
                Rules->sign = 1;
                Rules->type = 'n'; //好像可有可無
                break;
            }
            else 
            {
                temp2_Node = temp2_Node->left;
            }
        }
    }
    if (Rules->left) FunBody(IDs, Rules->left);
    if (Rules->right) FunBody(IDs, Rules->right);
}

functionNode makeFuncNode(Node Rule, Node Params, char* id)
{
    functionNode fn = (functionNode)malloc(sizeof(struct FUNCTIONNODE));
    
    fn->RULE = Rule;
    fn->PARAMS = Params;
    fn->id = id;

    return fn;
}

void PreOrder(Node n)
{
    printf("%c, %d, %s\n", n->type, n->num, n->id);
    if (n->left) PreOrder(n->left);
    if (n->right) PreOrder(n->right);
}

int main(int argc, char *argv[])
{
    yyparse();
    if (SYNTAXERROR == 1) return 0;
    // PreOrder(Origin);
    // printf("~~~~~~\n");
    Run(Origin);
    return(0);
}
