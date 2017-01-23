:- bundle(ciao_gsl).
version('0.1').
depends([
    core,
    ciaomath
]).
alias_paths([
    ciao_gsl = 'src'
]).
lib('src').
