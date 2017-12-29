' This subroutine solve the Model
subroutine solvemodel(string %solveopt)
  smpl {%baseyear} @last
  '%tracklist = "q_01 q_02"                 'Set the list of variable to be traCK
  %tracklist = "@all"                   'Set the list of variable to be traCK
  if %solveopt="u0" then
    {%modelname}.track {%traCKlist}         ' Specify endogenous variables to traCK (to be saved as series). To be used if memory or speed problem.
    {%modelname}.solve(o=b, g=10, m=5500, c=1e-8, z=1e-8,j=a,i=p,v=t)               ' Simulation of the model / OPTION :
    ' o= Algorithm solution method: g (Gauss-Seidel), n (NeWton), b(Broyden).
    ' g= Number of digits to round solution.
    ' m= Maximum number of iterations for solution (maximum 100 000)
    ' c= Maximum change in any of the endogenous variables (Convergence criterion) : 1e-15 and 0.01
    ' z= Positive number beloW Which the solution (absolute value) is set to zero, n (do not set to zero)
    ' j= arg (default=a). Use analytic or numeric Jacobians: a (analytic), n
    ' i= arg (default=a). Set initial (starting) solution values: "a" (actual), "p" (values in period prior to start of solution period)
    ' v=arg (default=="f") Display verbose diagnostic messages: "t" (true), "f" (false).

    return          ' Exit subroutine
  endif
  'ADD NEW USER OPTION IF NECESSARY WITH A "IF / END IF" SCRIPT
  if %solveopt="u1" then
    {%modelname}.track {%tracklist}         ' Specify endogenous variables to traCK (to be saved as series). To be used if memory or speed problem.
    {%modelname}.solve(o=g, g=10, m=5500, c=1e-8, z=1e-8,j=a)               ' Simulation of the model / OPTION : see above
    return          ' Exit subroutine
  endif
  if %solveopt="d" then

    %tracelist = ""
    For %sec {%list_sec}
      %tracelist  = %tracelist + " PIS_"+%sec+" IS_"+%sec+" PY_n_"+%sec+" PIA_"+%sec
    next

    {%modelname}.settrace {%tracelist}
    {%modelname}.track {%tracklist}     ' Specify endogenous variables to track (to be saved as series). To be used if memory or speed problem.
    {%modelname}.solve(o=b, g=10, m=5500, c=1e-8, z=1e-8,j=a,i=p,v=t)               ' Simulation of the model / OPTION :


    freeze({%modelname}_msg) {%modelname}.msg             'Display and freeze model solution messages
    freeze({%modelname}_block) {%modelname}.block           'Display and freeze the model bloCK structure view
    show {%modelname}_block
    show {%modelname}_msg
    {%modelname}.trace                                    'Display trace view of a model showing iteration history for selected solved variables.

  else
    {%modelname}.solve     ' Simulation of the model (Default option).
  endif
  smpl @all
endsub
