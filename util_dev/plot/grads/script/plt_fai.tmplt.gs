'reinit'

***************************************************************

*                                                             *

* GrADS script to plot the Product of RTMA3D                  *

*                                                             *

*   this gs file(plt_fai.tmplt.gs):                           *

*        used as a template gs file.                          *

*                                                             *

*   variables_list_for_plot.txt:                              *

*        used as control file to set up the variable          *

*        which is to be plotted.                              *

*                                                             *

*   Prgrmmer                 Date          Comment            *

*    Gang Zhao(NCEP/EMC)      2018-10-xx    Initial           *

*                                                             *

*                                                             *

*   History:                                                  *

*                                                             *

*                                                             *

***************************************************************

rc = gsfallow("on")

'q xinfo'

ret = result

line_wid = sublin(ret,1)

wid = subwrd(line_wid,4)

if(wid != '')

  say "This GrADS script is running interactively."

  say "    Its Window ID :"wid

endif

'open fgs_nat.ctl'

'open anl_nat.ctl'

* 'open fgs_prs.ctl'

* 'open anl_prs.ctl'

*'reset'

* set vpage with grads function panels 

panel_setup="2 2"

panels(panel_setup)

fname_head="GMFNAME"

* ready to plot

***************************************************************************************

* read in the variable names for plot

* in the following format:

* vname		vnamestr		factor	z-level 	vector		unit 		(this line is not in the list file)

* VISsfc	VisibiSFC		0.001	1		0 (scalar)	km

* ...		...			...

* ...		...			...

* RH2m		RelHumi2m		1.0	1		0		%

* ...		...			...

* WINDVECTORhlev	Wind-vector_z=1	1.0	1		1 (vector)	m/s

* ...		...			...

* VGRDhlev      VWindZ=50		1.0     50		0		m/s

***************************************************************************************

go_read=1

icount=0

icountall=0

icountskip=0

while(go_read>0)

  ret = read('./variables_list_for_plot.txt')

  status = sublin(ret,1)

  if(status != 0)

    if(icountall=0)

      say 'error: file variables_list_for_plot.txt is empty or does not exist.'

      return -1

      'quit'

    else

      if(icountall>=1)

        say 'reach the end of file variables_list_for_plot.txt'

        say 'total number of variables: 'icountall

        say 'total number of variables(plotted): 'icount

        say 'total number of variables(skipped): 'icountskip

        break

      endif

    endif

    go_read=-1

  else

    var_attribute=sublin(ret,2)

    cmmntmark=substr(var_attribute,1,1)

    if(icountall=0)

      say 'read in variable attibute: '

    endif

    icountall=icountall+1

    if(cmmntmark != "*" & cmmntmark != "!")

      icount=icount+1

      say '    --> 'var_attribute' : to be plotted '

      vname.icount=subwrd(var_attribute,1)

      vnamestr.icount=subwrd(var_attribute,2)

      factor.icount=subwrd(var_attribute,3)

      zlev.icount=subwrd(var_attribute,4)

      vector.icount=subwrd(var_attribute,5)

      vunit.icount=subwrd(var_attribute,6)

      fname.icount=fname_head%"_"%vnamestr.icount%".gmf"

      go_read=1

    else

      icountskip=icountskip+1

      say '    --> 'var_attribute' : to be skipped (not plotted) '

      go_read=1

    endif

  endif

endwhile

say ' total number of variables(to be plotted): 'icount

pnltitle.1="Fgs.Y4M2D2H2M2"

pnltitle.2="Anl."

pnltitle.3="Inc. (Anl - Fgs)"

nv=1

while(nv<=icount)

  vname_loc=vname.nv

* set up the map projection

* 'set mproj latlon'

  'set mproj scaled'

* 'set mproj lambert'

  p = 1

  while(p<4)

    _vpg.p

    'set grads off'

    'set grid off'

    if(vector.nv=1)

      'set gxout vector'

    else

      'set gxout shaded'

    endif

*   set the range of z (vertical coordinate)

*    'set z 'zlev.nv
    'set z '1

    if(p=1 | p=2)

*     fgs or anl

      if(vector.nv=1)

        if(vname.nv="WINDVECTOR10m")

*         wind vector @ 10 meters

          'd skip(UGRD10m.'p',40,25); skip(VGRD10m.'p',40,25)'

        endif

        if(vname.nv="WINDVECTORhlev")

*         wind vector @ model level (hlev)

          'd skip(UGRDhlev.'p',40,25); skip(VGRDhlev.'p',40,25)'

        endif

      else

*       scalar variables

        if(vname.nv="VISsfc")

*         Visibility (in unit of mile)

          'set clevs 0 1.0 3.0 5.0'

          'set ccols 0 2 8 3 4'

          'd 'vname.nv'.'p'*'factor.nv

        else

          if(vname.nv="CEIL")

*           Cloud Ceiling Height (in unit of ft)

            'set clevs 0 500.0 1000.0 3000.0'

            'set ccols 0 2 8 3 4'

            'd 'vname.nv'.'p'*'factor.nv

          else

            if(vname.nv="CEIL215")

*             Cloud Ceiling Height=L215-Lsfc (in unit of ft)

              if(p=1); vnm="cl215fgs"; endif

              if(p=2); vnm="cl215anl"; endif

              'define 'vnm'=HGTL215.'p'-HGTsfc.'p

              'set clevs 0 500.0 1000.0 3000.0'

              'set ccols 0 2 8 3 4'

              'd 'vnm'*'factor.nv

            else

              'd 'vname.nv'.'p'*'factor.nv

            endif

          endif

        endif

      endif

    else

*     inc

      if(vector.nv=1)

        if(vname.nv="WINDVECTOR10m")

*         wind vector @ 10 meters

          'd skip((UGRD10m.2-UGRD10m.1),40,25); skip((VGRD10m.2-VGRD10m.1),40,25)'

        endif

        if(vname.nv="WINDVECTORhlev")

*         wind vector @ model level (hlev)

          'd skip((UGRDhlev.2-UGRDhlev.1),40,25); skip((VGRDhlev.2-VGRDhlev.1),40,25)'

        endif

      else

        if(vname.nv="CEIL215")

*         Cloud Ceiling Height=L215-Lsfc (in unit of ft)

          'd (cl215anl-cl215fgs) * 'factor.nv

        else

          'd 'vname.nv'.2 * 'factor.nv' - 'vname.nv'.1 * 'factor.nv

        endif

      endif

    endif

    if(vector.nv!=1) ; 'run cbarn.gs' ; endif

    'draw title 'vnamestr.nv'('vunit.nv') of 'pnltitle.p

    p=p+1

  endwhile

  'gxprint '%fname.nvi' png'

  if(wid != '')

    say "plot is done for variable:"vnamestr.nv"  check the figure."

    say "hit any key to continue..."

    pull dummy

  endif

  'c'

  if(vname.nv="CEIL215")

    'undefine cl215fgs'

    'undefine cl215anl'

  endif

  nv = nv + 1

endwhile

*'disable print'

* exit grads (for batch job)

return 0

'quit'

