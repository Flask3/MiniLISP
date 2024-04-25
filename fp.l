%{
    #include "y.tab.h"
%}

number      [0]|[1-9][0-9]*|\-[1-9][0-9]*
id          [a-z]([a-z]|[0-9]|\-)*
lp          \(
rp          \)
add     \+
minus   \-
multiply \*
div     \/
larger \>
smaller \<
equal \=
and         and
or          or
not         not
mod         mod
if          if
printNum    print-num
printBool   print-bool
true    #t
false   #f
define      define
fun         fun

%%

{number}    { yylval.num = atoi(yytext); return(number);}
{printNum}  { return(printnum);}
{printBool} { return(printbool);}
{lp}        { return(lp);}      
{rp}        { return(rp);}   

{add}       {return(plus);}
{minus}     {return(minus);}
{multiply}  {return(mul);}
{div}       {return(division);}
{mod}       {return(mod);}
{larger}    {return(larger);}
{smaller}   {return(smaller);}
{equal}     {return(equal);}
{and}       {return(and);}
{or}        {return(or);}
{not}       {return(not);}
{true}      {yylval.num = 1; return(true);}
{false}     {yylval.num = 0; return(false);}
{define}    {return(define);}
{if}        {return(If);}
{fun}       {return(fun);}
{id}        { yylval.word = strdup(yytext); return(id);}

[\r] {}
[\t] {}
[\n]  {}
[ ] {}
. {}
%%