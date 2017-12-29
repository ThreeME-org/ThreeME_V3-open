subroutine load_realist()
  vector(1) vectnb_4

  vectnb_4.read(a1,s=exo_realistic_1) {%data_calibration} 1
  !exo_realistic=vectnb_4(1)
  read(c2,s=exo_realistic_1,t) {%data_calibration} !exo_realistic


 vectnb_4.read(a1,s=exo_realistic_Hybrid) {%data_calibration} 1
 !exo_realistic=vectnb_4(1)
 read(c2,s=exo_realistic_Hybrid,t) {%data_calibration} !exo_realistic

endsub