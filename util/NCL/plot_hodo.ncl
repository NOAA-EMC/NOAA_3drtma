;-----------------------------------------------------------------------------------
;
function plot_hodo(wks:graphic, u:numeric, v:numeric, z:numeric, \
                   ust:numeric, vst:numeric, HodoRes:logical)
;
;-----------------------------------------------------------------------------------
;
; PLOT A HODOGRAPH GIVEN U, V, & Z
;
; Inputs
;
;      wks       - valid workstation reference
;
;      u,v       - x/y wind speed in m/s
;
;      z         - height in meters
;
;      ust,vst   - storm motion components in m/s   ; added - BJ
;
;      HodoRes   - resources to control scaling of plot, plot parameters, draw, frame, etc.
;
;                  @circFr      - intervals at which frequency circles are drawn
;                                 eg: circFr=10. is common
;                  @Title       - title of plot
;                  @SpdMax      - maximum velocity of the plot
;                  @zMax        - maximum height to plot
;                  @gsnDraw     - draw control
;                  @gsnFrame    - frame control
;                  @DashPattern - pattern for hodograph lines
;
; Outputs
; 
; If gsnDraw/Frame = True, then a plot is drawn.
;
; plot_hodo also returns a plot object which can be used to create panel plots, etc.
;
;-----------------------------------------------------------------------------------
; BEGIN SCRIPT

begin

  debug = False

; Plotting parameters

  popts          = True                 ; local and default options
  popts@gsnDraw  = True
  popts@gsnFrame = True
  popts@circFr   = 20.0                 ; default circle size
  popts@Title    = "HODOGRAPH"
  popts@SpdMax   = 80.                  ; max wind speed for hodograph (or so..)
  popts@zMax     = 10000.               ; max height to be drawn
  popts@DashPattern = 0                 ; solid lines

  if (HodoRes) then
   popts = HodoRes     ; if True over ride local/defaults with input
  end if

  circFr = popts@circFr
  SpdMax = popts@SpdMax
  zMax   = popts@zMax

  nW     = dimsizes(u   )       ; total number of elements
  rad    = 4.*atan(1.0)/180.    ; degress to radians to compute wind components

; Draw hodograph
  
  SpdMaxNumCircles = floattointeger(SpdMax/circFr)
  SpdMaxNum        = circFr*SpdMaxNumCircles   ; number of circles

  nCirc = 361                         ; do this once
  xCirc = new ( (/SpdMaxNumCircles,nCirc /) , float)
  yCirc = xCirc
  xcos  = cos(fspan(0, 360, nCirc)*rad)*circFr
  xsin  = sin(fspan(0, 360, nCirc)*rad)*circFr

  do n=0,SpdMaxNumCircles-1         ; plot coordinates for freq circles
     xCirc(n,:) = (n+1)*xcos(:)
     yCirc(n,:) = (n+1)*xsin(:)
  end do

  delete(xcos)
  delete(xsin)
                                                    ; Specify data limits for X and Y axes.
  extraSpace             = max((/3.,circFr/3./))    ; Extra space beyond outer circle
  opts                   = True
  opts@trXMinF           = -SpdMaxNum-extraSpace  ; min X 
  opts@trXMaxF           =  SpdMaxNum+extraSpace  ; max X
  opts@trYMinF           = -SpdMaxNum-extraSpace  ; min Y
  opts@trYMaxF           =  SpdMaxNum+extraSpace  ; max Y
  opts@tmXTOn            = False                    ; turn off tick marks on each side
  opts@tmXBOn            = False
  opts@tmYLOn            = False
  opts@tmYROn            = False
  opts@tmXBBorderOn      = False                    ; turn off borders on each side
  opts@tmXTBorderOn      = False
  opts@tmYLBorderOn      = False
  opts@tmYRBorderOn      = False
  opts@gsnFrame          = False                    ; do not advance frame
  opts@gsnDraw           = False                    ; do not draw
  opts@xyMonoDashPattern = True                     ; set all circles to solid
  opts@xyLineColor       = "black"                  ; set all circles to solid
 
; White background for the inset hodograph - BJ
  gsRes                  = True
  gsRes@gsFillColor      = "White"
  gsn_polygon_ndc(wks,(/0.072,0.34,0.34,0.072/),(/0.924,0.924,0.64,0.64/),gsRes)
  delete(gsRes)

  plotHodo               = gsn_xy(wks,xCirc,yCirc,opts) ; GSUN routine to draw circles 

; Label Rings

  label                   = new (1,string)          ; pre-define variable of type string
  txRes                   = True                    ; set text resources
  txRes@txFontHeightF     = 0.03
  txRes@txAngleF          = 45.
  gsRes = True                                      ; set polyline resources
  gsRes@gsLineThicknessF  = 2.0                     ; Extra thick lines
  gsRes@gsLineColor       = "gray"                  ; Extra thick lines
 
;  do n=1,SpdMaxNumCircles-1                       ; label circles
   n=SpdMaxNumCircles-1                            ; label outermost circle
   xP    = -1. + (n+1)*circFr*cos(135.*rad)
   yP    = 1.0 + (n+1)*circFr*sin(135.*rad)
   xP = xP - 2.
   yP = yP + 2.
   label = floattointeger((n+1)*circFr)+" kts"  
   astring = systemfunc("echo text1$$")
   plotHodo@$astring$ = gsn_add_text(wks,plotHodo,label,xP,yP,txRes) ; add text at end
;  end do

; Add horizontal and vertical axes...

  xP                 = 1.6*SpdMax*sin(0.0*rad)         
  yP                 = 1.6*SpdMax*cos(0.0*rad)
  astring            = systemfunc("echo line1$$")
  plotHodo@$astring$ = gsn_add_polyline(wks,plotHodo,(/-xP,xP/),(/-yP,yP/),gsRes) ; draw spoke

  xP                 = 1.6*SpdMax*sin(90.0*rad)         
  yP                 = 1.6*SpdMax*cos(90.0*rad)
  astring            = systemfunc("echo line1$$")
  plotHodo@$astring$ = gsn_add_polyline(wks,plotHodo,(/-xP,xP/),(/-yP,yP/),gsRes) ; draw spoke

; Convert wind components to knots - BJ

  u = u * 1.9429
  v = v * 1.9429
  ust = ust * 1.9429
  vst = vst * 1.9429

; Plot dot to indicate storm motion vector - BJ

  astring = systemfunc("echo data0$$")
  gsRes@gsMarkerIndex = 16
  gsRes@gsMarkerColor = "sienna1"
  gsRes@gsMarkerSizeF = 0.01
  plotHodo@$astring$      = gsn_add_polymarker(wks,plotHodo,ust,vst,gsRes)

; Plot data below 1 km

  i1 = closest_val(1000.,z)
  u1 = u(0:i1(0))
  v1 = v(0:i1(0))

  gsRes@gsLineThicknessF  = 3.0                ; Extra thick lines
  gsRes@gsLineColor       = "red"
  gsRes@gsLineDashPattern = popts@DashPattern
  astring = systemfunc("echo data1$$")
  plotHodo@$astring$      = gsn_add_polyline(wks,plotHodo,u1,v1,gsRes) ; Plot hodo...

; Plot data between 1 km and 3 km

  i3 = closest_val(3000.,z)
  u3 = u(i1(0):i3(0)) 
  v3 = v(i1(0):i3(0))

  gsRes@gsLineThicknessF  = 3.0                ; Extra thick lines
  gsRes@gsLineColor       = "cyan3"
  astring = systemfunc("echo data2$$")
  plotHodo@$astring$      = gsn_add_polyline(wks,plotHodo,u3,v3,gsRes) ; Plot hodo...

; Plot data between 3 km and Zmax

  iz = closest_val(zMax,z)
  uz = u(i3(0):iz(0))
  vz = v(i3(0):iz(0))

  gsRes@gsLineThicknessF  = 3.0                ; Extra thick lines
  gsRes@gsLineColor       = "black"
  astring = systemfunc("echo data2$$")
  plotHodo@$astring$      = gsn_add_polyline(wks,plotHodo,uz,vz,gsRes) ; Plot hodo...

  txRes@txAngleF = 0.0
  txRes@txFontHeightF= 0.0120

  do n=0,iz(0)
     xP    = u(n) - 2.5
     yP    = v(n) + 2.5
     label = floattointeger(z(n)/100.)
     astring = systemfunc("echo label2$$")
;     plotHodo@$astring$ = gsn_add_text(wks,plotHodo,label,xP,yP,txRes) ; add text at end
  end do

; legend resources

 lgres                    = True
 lgres@lgLineColors       = (/"black","cyan3","red"/)    ; colors for legend lines
 lgres@vpWidthF           = 0.15                   ; width of legend (NDC)
 lgres@vpHeightF          = 0.05                  ; height of legend (NDC)
 lgres@lgPerimOn          = False                 ; turn off perimeter
 lgres@lgDashIndexes      = (/0,0,0/)           ; dash indexes
 lgres@lgLineLabelStrings = (/"","",""/)       ; no labels in lines
 lgres@lgLineThicknessF = 3.0
 lgres@lgBoxMinorExtentF = 0.2

 lgres@lgOrientation      = "Horizontal"
; lgres@lgItemPositions    = (/0.2,0.2,0.2/)
 lgres@lgItemPositions    = (/0.0,0.0,0.0/)
; lgres@lgLabelAngleF      = 90.
 lgres@lgLabelFontHeightF = 0.02

 zmax_km = round(zMax / 1000.0, 2)
; gsn_legend_ndc(wks,3,(/ "3 km < z < " + zmax_km + " km","1 km < z < 3 km","0 km < z < 1 km " /),0.1,0.725,lgres)

; Custom hard-wired legend - BJ

  tres = True
  tres@txFontHeightF = 0.01
  gsn_text_ndc(wks,"0",0.08,0.65,tres)
  gsn_text_ndc(wks,"1",0.14,0.65,tres)
  gsn_text_ndc(wks,"3",0.205,0.65,tres)
  gsn_text_ndc(wks,"10 km AGL",0.3,0.65,tres)
  delete(tres)
  plres = True
  plres@gsLineThicknessF  = 3.0                ; Extra thick lines
  plres@gsLineColor       = "red"
  gsn_polyline_ndc(wks,(/0.09,0.13/),(/0.65,0.65/),plres)
  plres@gsLineColor       = "cyan3"
  gsn_polyline_ndc(wks,(/0.15,0.195/),(/0.65,0.65/),plres)
  plres@gsLineColor       = "black"
  gsn_polyline_ndc(wks,(/0.215,0.26/),(/0.65,0.65/),plres)
  delete(plres)

; label the Plot

  tres               =  True
  tres@txFontHeightF = 0.02
;  gsn_text_ndc(wks,popts@Title,0.5,0.85,tres)
  tres@txFontHeightF = 0.0125
;  gsn_text_ndc(wks,"(Z in decameters)",0.7,0.225,tres)

  if (isatt(popts,"gsnDraw")) then 
      if (popts@gsnDraw) then 
          draw(plotHodo)
      end if
  end if

  if (isatt(popts,"gsnFrame")) then 
    if (popts@gsnFrame) then 
        frame (wks)
    end if
  end if

  return (plotHodo)

 end
