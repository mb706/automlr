

## Test column types influence behaivour of wrappers and learners in the correct way
 ## - [ ] has.X, removes.X
 ##   - [ ] has.X:
 ##     - [ ] parameters in learner can depend on 'has.X'. X may be missings, factors, ordered but not numerics. this is so for
 ##       - [ ] .AMLRFIX
 ##         - [ ] parameters with .AMLRFIX, fixed: value takes on this value in presence/absence of X
 ##         - [ ] parameters with .AMLRFIX, variable: two external vars, each only valid some times, set the goal variable differently inside the learner.
 ##       - [ ] normal variables
 ##         - [ ] removed (or requirement removed) if it trivially depends on has.X / removes.X and requirement satisfied n / y
 ##     - [ ] how variable
 ##       - [ ] is fixed to NO if the task doesn't have X to begin with
 ##       - [ ] is fixed to YES if the task has it an nothing can convert
 ##       - [ ] there is an external var determining this if X is in the task and a wrapper can remove it
 ##       - [ ] there is another var only available if the above is TRUE, determining which wrapper removes it, if more than one wrapper are present
 ##       - [ ] removes.X is set for the given wrapper. this has influence on .AMLRFIX for fixed and variable learners and changes the external search space accordingly, while setting internally the variable accordingly
 ##       - [ ] has.X is true for all the wrappers before and including the one removing it, is false afterwards; .AMLRFIX etc. behaves accordingly

