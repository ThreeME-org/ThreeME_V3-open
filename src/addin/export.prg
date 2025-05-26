' Export all variables to a csv file
' This is used to communicate with the compiler
subroutine export_all_to_csv
  smpl {%baseyear} {%baseyear}

  delete(noerr) all_vars_table
  group all_vars *
  freeze(all_vars_table) all_vars
  all_vars_table.save(t=csv) .\model\_tmp_all_vars.csv

  smpl @all
endsub