Global Dim rex.f(512*2+1)
Global Dim imx.f(512*2+1)
Global Dim OutPutArray.f(512*2+1)
Global Dim OutPutNoteArray.s(512*2+1)

Global Dim Notes.s(53)
Global t.i
#SAMPLE_RATE = 40000 ;44100 ;8000 ;LIMIT 4000 Hz AUDIO
#gadText1 = 1
Global FFTWnd

;DECLARE THESE OUTSIDE PROCEDURE
Global N.i=1024   ; // Number of samples
Global M.i=10     ; // If N.w = 1024 == 10 == Same as:  Int(Log(N) / 0.69314718055994529)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
Enumeration 
  #JSONFile
EndEnumeration

;Info: MCI-MP3-Commands
Enumeration 0
  #MP3_Unknown
  #MP3_Stopped
  #MP3_Playing
  #MP3_Paused
EndEnumeration

Procedure MP3_GetStatus(Nb)
  Result=#MP3_Unknown
  a$=Space(#MAX_PATH)
  i=mciSendString_("status MP3_"+Str(Nb)+" mode",@a$,#MAX_PATH,0)
  If i=0
    Debug a$
    Select a$
      Case "stopped":Result=#MP3_Stopped
      Case "playing":Result=#MP3_Playing
      Case "paused":Result=#MP3_Paused
    EndSelect
  EndIf
  ProcedureReturn Result
EndProcedure
Procedure MP3_Load(Nb,file.s)
  ;i=mciSendString_("open Sequencer!"+Chr(34)+file+Chr(34)+" alias mid"+Str(Nb),0,0,0)
  i=mciSendString_("OPEN "+Chr(34)+file+Chr(34)+" Type MPEGVIDEO ALIAS MP3_"+Str(Nb),0,0,0)
  If i=0
    ProcedureReturn #True
  Else
    ProcedureReturn #False
  EndIf
EndProcedure
Procedure MP3_Play(Nb)
  i=mciSendString_("play MP3_"+Str(Nb),0,0,0)
  ProcedureReturn i
EndProcedure
Procedure MP3_PlayStart(Nb)
  i=mciSendString_("play MP3_"+Str(Nb)+" from "+Str(0),0,0,0)
  ProcedureReturn i
EndProcedure
Procedure MP3_PlayPart(Nb,Start,endPos)
  i=mciSendString_("play MP3_"+Str(Nb)+" from "+Str(Start)+" to "+Str(endPos),0,0,0)
  ProcedureReturn i
EndProcedure
Procedure MP3_Pause(Nb)
  i=mciSendString_("pause MP3_"+Str(Nb),0,0,0)
  ProcedureReturn i
EndProcedure
Procedure MP3_Resume(Nb)
  i=mciSendString_("resume MP3_"+Str(Nb),0,0,0)
  ProcedureReturn i
EndProcedure
Procedure MP3_Stop(Nb)
  i=mciSendString_("stop MP3_"+Str(Nb),0,0,0)
  ProcedureReturn i
EndProcedure
Procedure MP3_Free(Nb)
  i=mciSendString_("close MP3_"+Str(Nb),0,0,0)
  ProcedureReturn i
EndProcedure
Procedure MP3_SetVolume(Nb,volume)
  i=mciSendString_("SetAudio MP3_"+Str(Nb)+" volume to "+Str(volume),0,0,0)
  ProcedureReturn i
EndProcedure
Procedure MP3_GetVolume(Nb)
  a$=Space(#MAX_PATH)
  i=mciSendString_("status MP3_"+Str(Nb)+" volume",@a$,#MAX_PATH,0)
  ProcedureReturn Val(a$)
EndProcedure


Procedure MP3_SetSpeed(Nb,Tempo)
  i=mciSendString_("set MP3_"+Str(Nb)+" Speed "+Str(Tempo),0,0,0)
  ProcedureReturn i
EndProcedure
Procedure MP3_GetSpeed(Nb)
  a$=Space(#MAX_PATH)
  i=mciSendString_("status MP3_"+Str(Nb)+" Speed",@a$,#MAX_PATH,0)
  ProcedureReturn Val(a$)
EndProcedure
Procedure MP3_GetLength(Nb)
  a$=Space(#MAX_PATH)
  i=mciSendString_("status MP3_"+Str(Nb)+" length",@a$,#MAX_PATH,0)
  ProcedureReturn Val(a$)
EndProcedure
Procedure MP3_GetPosition(Nb)
  a$=Space(#MAX_PATH)
  i=mciSendString_("status MP3_"+Str(Nb)+" position",@a$,#MAX_PATH,0)
  ProcedureReturn Val(a$)
EndProcedure
Procedure MP3_Seek(Nb,pos)
  i=mciSendString_("Seek MP3_"+Str(Nb)+" to "+Str(pos),0,0,0)
  ProcedureReturn i
EndProcedure
Procedure.s MP3_TimeString(Time)
  Time/1000
  sek=Time%60:Time/60
  min=Time%60:Time/60
  ProcedureReturn RSet(Str(Time),2,"0")+":"+RSet(Str(min),2,"0")+":"+RSet(Str(sek),2,"0")
EndProcedure
;Example

Enumeration 1
  #gadget_File
  #Gadget_VolumeTxt
  #Gadget_Volume
  #Gadget_SpeedTxt
  #Gadget_Speed
  #Gadget_PositionTxt
  #Gadget_Position
  #Gadget_Load
  #Gadget_Play
  #Gadget_Stop
  #Gadget_Pause
  #Gadget_Resume
EndEnumeration

Procedure SetVol(x)
  SetGadgetText(#Gadget_VolumeTxt,"Volume:"+Str(x))
  SetGadgetState(#Gadget_Volume,x)
EndProcedure
Procedure SetSpeed(x)
  SetGadgetText(#Gadget_SpeedTxt,"Speed:"+Str(x))
  SetGadgetState(#Gadget_Speed,x)
EndProcedure
Procedure SetPosition(x,max)
  SetGadgetText(#Gadget_PositionTxt,"Position:"+MP3_TimeString(x)+" : "+MP3_TimeString(max))
  If max>0
    SetGadgetState(#Gadget_Position,x*1000/max)
  Else
    SetGadgetState(#Gadget_Position,0)
  EndIf
EndProcedure

Structure NoteRange
   Note.i
   FromPos.i
   ToPos.i
EndStructure

Procedure.i ShowNote_Init()
   
   Global Dim NoteRange.NoteRange(53)
   
   For Note=0 To 53
      Read.w FromPos.w
      Read.w ToPos.w
      NoteRange(Note)\FromPos=FromPos
      NoteRange(Note)\ToPos=ToPos
      NoteRange(Note)\Note=Note
   Next
   
   Global Dim g_RealNote.s(53)
   
   For i=0 To 42+12-1
      Read.s sRealNote.s
      g_RealNote(i)=sRealNote.s
   Next
EndProcedure

Procedure.s ShowNote_Get(lValue)
   ProcedureReturn g_RealNote.s(lValue)
 EndProcedure
 
 ShowNote_Init()
 
 Structure SCOPE
   channel.b
   left.i
   top.i
   width.i
   height.i
   middleY.i
   quarterY.i
EndStructure

Structure CONFIG
   
   hWindow.i           ; Window handle
   size.i              ; Wave buffer size
   buffer.i            ; Wave buffer pointer
   output.i            ; WindowOutput()
   wave.i              ; Address of waveform-audio input device
   
   format.WAVEFORMATEX ; Capturing WaveFormatEx
   lBuf.i              ; Capturing Buffer size
   nBuf.i              ; Capturing Buffer number
   nDev.i              ; Capturing Device identifier
   nBit.i              ; Capturing Resolution (8/16)
   nHertz.i            ; Capturing Frequency  (Hertz)
   nChannel.i          ; Capturing Channels number (Mono/Stereo)
   
   LScope.SCOPE        ; Wave form display
   RScope.SCOPE        ; Wave form display
   
EndStructure

Global Config.CONFIG
Global Dim inHdr.WAVEHDR(16)

Config\format\wFormatTag=#WAVE_FORMAT_PCM

Procedure Record_Start()
  
   Config\format\nChannels=1
   
   Config\format\wBitsPerSample=16
   Config\format\nSamplesPerSec=8000
   Config\nDev=0 ; (0 default MS Sound Mapper device)
   Config\lBuf=1024
   Config\nBuf=8
   Config\nBit=1
   
   Config\format\nBlockAlign=(Config\format\nChannels*Config\format\wBitsPerSample)/8
   Config\format\nAvgBytesPerSec=Config\format\nSamplesPerSec*Config\format\nBlockAlign
   

   
   If #MMSYSERR_NOERROR=waveInOpen_(@Config\wave, #WAVE_MAPPER+Config\nDev, @Config\format, Config\hWindow, #Null, #CALLBACK_WINDOW | #WAVE_FORMAT_DIRECT)
      
      For i=0 To Config\nBuf-1
         inHdr(i)\lpData=AllocateMemory(Config\lBuf)
         inHdr(i)\dwBufferLength=Config\lBuf
         waveInPrepareHeader_(Config\wave, inHdr(i), SizeOf(WAVEHDR))
         waveInAddBuffer_(Config\wave, inHdr(i), SizeOf(WAVEHDR))   
      Next
      
      If #MMSYSERR_NOERROR=waveInStart_(Config\wave)
        SetTimer_(Config\hWindow, 0, 1, 0)
       EndIf
       
   EndIf   
EndProcedure

Procedure Record_Read(hWaveIn.i, lpWaveHdr.i)
   *hWave.WAVEHDR=lpWaveHdr
   Config\buffer=*hWave\lpData
   Config\size=*hWave\dwBytesRecorded
   waveInAddBuffer_(hWaveIn, lpWaveHdr, SizeOf(WAVEHDR))
EndProcedure

Procedure record_FindNote(Value)
   For Note=0 To 53
      If Value=>NoteRange(Note)\FromPos And Value<=NoteRange(Note)\ToPos
         ProcedureReturn note
      EndIf
   Next
EndProcedure

Procedure record_doFFT(*scope.SCOPE)
   
   Define.d TR, TI, SR, SI, UR, UI
   Define.i J, K, L, NM1, ND2, cnt
   Define.i MaxPeak, MaxValue, DiffY
   Define.w value
   Define MaxIndex.i
   
   ; // -------- Init some values for FFT analysing --------
   
   ;    N   = 1024                  ; // Number of samples
   NM1=N-1
   ND2=N>>1                         ; // Optmimized, instead N / 2
   ;    M   = 10                    ; // If N.w = 1024 == 10 == Same as:  Int(Log(N) / 0.69314718055994529)
   J=ND2
   
   If Config\buffer=0 : ProcedureReturn : EndIf
   
   ; // -------- Clear and Fill array values for analysing in just only one loop --------
   
   For i=0 To Config\size Step 2       ; // Optimized by merging clear and fill array in one loop
      rex(i>>1)=0             ; //   0 to  512
      imx(i>>1)=0             ; //   0 to  512
      rex(i>>1+N>>1)=0        ; // 513 to 1024
      imx(i>>1+N>>1)=0        ; // 513 to 1024
      
      value=PeekW(Config\buffer+i)
      ;       value.w    = PeekW( Config\buffer + i + *scope\channel * 2 )  ; // Enable this For Stereo Inpus
      rex(i>>1)=value/32767   ; // Optimized by doing i >> 1
   Next
   
   ; // -------- Start FFT --------
   
   For i.i=1 To N-2           ; // Bit reversal sorting
      If i<J
         TR=REX(J)
         TI=IMX(J)
         REX(J)=REX(i)
         IMX(J)=IMX(i)
         REX(i)=TR
         IMX(i)=TI
      EndIf
      
      K=ND2
      
      While K<=J
         J=J-K
         K=K>>1                                    ; // Optmimized, instead N / 2
      Wend
      J=J+K
   Next
   
   For L=1 To M                                    ; // Loop for each stage
      LE.i=1<<L                                    ; // Optimized, instead  LE.i = Int( Pow( 2, L ) )
      LE2.i=LE>>1                                  ; // Optimized, instead  N / 2
      UR=1
      UI=0
      SR=Cos(#PI/LE2)                              ; // Calculate sine & cosine values
      SI=-Sin(#PI/LE2)
      For J.i=1 To LE2                             ; // Loop for each sub DFT
         JM1.i=J-1
         For i=JM1 To NM1                          ; // Loop for each butterfly
            IP.i=i+LE2
            TR=REX(IP)*UR-IMX(IP)*UI               ; // Butterfly calculation
            TI=REX(IP)*UI+IMX(IP)*UR
            REX(IP)=REX(i)-TR
            IMX(IP)=IMX(i)-TI
            REX(i)=REX(i)+TR
            IMX(i)=IMX(i)+TI
            i+LE-1
         Next i
         TR=UR
         UR=TR*SR-UI*SI
         UI=TR*SI+UI*SR
      Next
    Next
    
  
   ; // -------- Calculate Outputarray and search for MaxValue of the Paket --------
   
    maxvalue=0 
    MaxIndex = 0
    ; // Optimized by merging calculate Outputarray
   ; // and search MaxValue into just one loop.
   For cnt=0 To N                                        ; // fixed to N.w instead wrong fixed value
      outputarray(cnt)=(IMX(cnt)*IMX(cnt))+(REX(cnt)*REX(cnt))
      If maxvalue<outputarray(cnt)
        maxvalue=outputarray(cnt)
        MaxIndex = cnt
      EndIf
    Next
    
    
    If (MaxIndex>0) And (MaxIndex<N-1)   
              y1.d = OutPutArray(MaxIndex-1)
              y2.d = OutPutArray(MaxIndex)
              y3.d = OutPutArray(MaxIndex+1)
              delta.d = (y3 - y1) / (2 * (2 * y2 - y1 - y3))          
              freq = (MaxIndex * (#SAMPLE_RATE / 1024)) + (delta*(#SAMPLE_RATE / 1024))            
            Else   
              freq = MaxIndex * (#SAMPLE_RATE / 1024)
            EndIf
            
   
   ; // -------- Draw FFT --------
   
   ;    StartDrawing( WindowOutput( FFTWnd ) ) ;NO NEED TO CALL THIS EVERY TIME
            Box(0, 0, N, 500, $0)
            MaxPeak=0
   For cnt=0 To 500 ;                                   ; // Change fixed value to N.w !?
      DiffY=Outputarray(cnt)/MaxValue*400
      Box(cnt, 400, 1, -DiffY, $FFFFFF)
      If DiffY>MaxPeak
         MaxPeak=DiffY
         MaxPos=cnt
      EndIf
   Next
   ;    StopDrawing() ;NO NEED TO CALL THIS EVERY TIME
   
 
   
   SetWindowTitle(FFTWnd, ShowNote_Get(record_FindNote(MaxPos)))
   SetGadgetText(#gadText1, "Frequency: " + Str(freq) + " Hz") 
   OutPutNoteArray(t)=ShowNote_Get(record_FindNote(MaxPos))
   t = t+1
EndProcedure

Procedure record_CallBack(hWnd.i, Msg.i, wParam.i, lParam.i)
   
   Result.i=#PB_ProcessPureBasicEvents
   
   Select Msg
     Case #WM_TIMER : record_doFFT(Config\LScope)
         ;CreateJSON(#JSONFile)
         ;InsertJSONArray(JSONValue(#JSONFile), OutPutNoteArray())
     Case #MM_WIM_DATA : record_Read(wParam, lParam)
   EndSelect
   
   ProcedureReturn Result
 EndProcedure
 
FFTWnd=OpenWindow(#PB_Any, 0, 0, 500, 500, "", #PB_Window_ScreenCentered | #PB_Window_SystemMenu)
Config\hWindow=WindowID(FFTWnd)
Config\output=WindowOutput(FFTWnd)
Global t=0
SetWindowCallback(@record_CallBack())

StartDrawing(WindowOutput(FFTWnd))
TextGadget(#gadText1, 10, 410, 110, 20, "Frequency: ") ;DISPLAYS APPROXIMATE FREQUENCY


If OpenWindow(0, 100, 200, 310,260, "MELODY CATCHER", #PB_Window_SystemMenu |#PB_Window_ScreenCentered)
  ;If CreateGadgetList(WindowID())
  top=5
  TextGadget    (#gadget_File       ,5,top,300,20,"File:"):top+25
  TextGadget    (#Gadget_VolumeTxt,  5,top,300,20,"Volume"):top+20
  TrackBarGadget(#Gadget_Volume     ,5,top,300,25,0,100):top+30
  TextGadget    (#Gadget_SpeedTxt   ,5,top,300,20,"Speed"):top+20
  TrackBarGadget(#Gadget_Speed      ,5,top,300,25,0,200):top+30
  TextGadget    (#Gadget_PositionTxt,5,top,300,20,"Position"):top+20
  TrackBarGadget(#Gadget_Position   ,5,top,300,25,0,1000):top+30
  ButtonGadget  (#Gadget_Load       ,5,top,300,20,"Load"):top+25
  ButtonGadget  (#Gadget_Play       ,5,top,300,20,"Play"):top+25
  ButtonGadget  (#Gadget_Stop      ,5,top,300,20,"Stop"):top+25
  ;ButtonGadget  (#Gadget_Resume     ,5,top,300,20,"Resume"):top+25
  ;ButtonGadget  (#Gadget_Stop       ,5,top,300,20,"Stop"):top+25
  loaded=#False
  Quit=#False
  
  Repeat
    EventID = WindowEvent()
    
    Select EventID
      Case 0
        If loaded And max>0
          x=MP3_GetPosition(1)
          If GetGadgetState(#Gadget_Position)<>x*1000/max
            SetPosition(x,max)
          EndIf
        EndIf
        Delay(100)
      Case #PB_Event_CloseWindow ; If the user has pressed on the close button
        Quit=#True
      Case #PB_Event_Gadget
        Select EventGadget()
          Case #Gadget_Load
            File$=OpenFileRequester("","","Media (Wave,MP3,OGG)|*.wav;*.ogg;*.mp3|Wave|*.wav|mp3|*.mp3|OGG|*.OGG|ALL|*.*",0)
            If File$<>""
              If loaded
                MP3_Free(1)
                loaded=#False
              EndIf
              If MP3_Load(1,File$)
                max=MP3_GetLength(1)
                SetVol(MP3_GetVolume(1)/10)
                SetSpeed(MP3_GetSpeed(1)/10)
                SetPosition(0,max)
                loaded=#True
                SetGadgetText(#gadget_File,"File:"+File$)
              Else
                SetGadgetText(#gadget_File,"File")
              EndIf
            EndIf
          Case #Gadget_Resume
            If loaded
              MP3_Resume(1)
            EndIf
          Case #Gadget_Pause
            If loaded
              MP3_Pause(1)
            EndIf
          Case #Gadget_Play
            If loaded
              MP3_Play(1)
              Record_Start()
              Repeat : Until #Gadget_Stop            
            EndIf
          Case #Gadget_Stop
            If loaded
              MP3_Stop(1)
              ;ReDim OutPutNoteArray.s(Len(File$))
              ;Debug Len(File$)
              ;OutPutNoteArray(t)="!"
              ReDim OutPutNoteArray(t-1)
              CreateJSON(#JSONFile)
              InsertJSONArray(JSONValue(#JSONFile), OutPutNoteArray())
              ;Debug AF_TimeString(AF_GetLength(1))
              SaveJSON(#JSONFile, "joy.txt")
              Debug ComposeJSON(#JSONFile)
              StopDrawing()
              End
            EndIf
          Case #Gadget_Position
            If loaded And max>0
              x=GetGadgetState(#Gadget_Position)*max/1000
              SetPosition(x,max)
              MP3_Seek(1,x)
              MP3_Resume(1)
            EndIf
          Case #Gadget_Volume
            If loaded
              x=GetGadgetState(#Gadget_Volume)
              SetVol(x)
              MP3_SetVolume(1,x*10)
            EndIf
          Case #Gadget_Speed
            If loaded
              x=GetGadgetState(#Gadget_Speed)
              SetSpeed(x)
              MP3_SetSpeed(1,x*10)
            EndIf
        EndSelect
    EndSelect
  Until Quit
  If loaded
    MP3_Stop(1)
    MP3_Free(1)
  EndIf
EndIf

For i = 0 To Config\nBuf - 1
  FreeMemory(inHdr(i)\lpData) ;FREE ALLOCATED MEMORY
Next

DataSection
   Notes:
   Data.w 14, 17 
   Data.w 18, 18 
   Data.w 19, 19
   Data.w 20, 20
   Data.w 21, 21
   Data.w 22, 23
   Data.w 24, 24
   Data.w 25, 26
   Data.w 27, 27
   Data.w 28, 29
   Data.w 30, 31
   Data.w 32, 32
   
   Data.w 32, 34.5 ;C1
   Data.w 34.5, 36.6 ;C#1
   Data.w 36.6, 38.8 ;D1
   Data.w 38.8, 41 ;D#1
   Data.w 41, 43.6 ;E1
   Data.w 43.6, 46 ;F1
   Data.w 46, 49 ;F#1
   Data.w 49, 52 ;G1
   Data.w 52, 55 ;G#1
   Data.w 55, 58.2 ;A1
   Data.w 58.2, 61.7 ;A#1
   Data.w 61.7, 65 ;B1
   
   Data.w 65, 69 ;C2
   Data.w 69, 73 ;C#2
   Data.w 73, 77  ;D2
   Data.w 77, 82 ;D#2
   Data.w 82, 87 ;E2
   Data.w 87, 92 ;F2
   Data.w 92, 97 ;F#2
   Data.w 97, 103 ;G2
   Data.w 103, 110 ;G#2
   Data.w 110, 116 ;A2
   Data.w 116, 123 ;A#2
   Data.w 123, 130 ;B2
   
   Data.w 130, 138.5 ;C3
   Data.w 138.5, 146.8 ;C#3
   Data.w 146.8, 155.5 ;D3
   Data.w 155.5, 164.8 ;D#3
   Data.w 164.8, 174.6 ;E3
   Data.w 174.6, 185 ;F3
   Data.w 185, 196 ;F#3
   Data.w 196, 207.6 ;G3
   Data.w 207.6, 220 ;G#3
   Data.w 220, 233 ;A3
   Data.w 233, 247 ;A#3
   Data.w 247, 261.6 ;B3
   
   Data.w 261.6, 277
   Data.w 277, 293.6
   Data.w 293.6, 311
   Data.w 311, 329.6
   Data.w 329.6, 349.2
   Data.w 349.2, 367
   RealNotes:
   Data.s                    "C", "C#", "D", "D#", "E", "F", "F#", "G", "G#"
   Data.s "A", "A#", "B", "C", "C#", "D", "D#", "E", "F", "F#", "G", "G#"
   Data.s "A", "A#", "B", "C", "C#", "D", "D#", "E", "F", "F#", "G", "G#"
   Data.s "A", "A#", "B", "C", "C#", "D", "D#", "E", "F", "F#", "G", "G#"
   Data.s "A", "A#", "B", "C", "C#", "D", "D#", "E", "F"
 EndDataSection
 
 
; IDE Options = PureBasic 5.60 (Windows - x86)
; CursorPosition = 600
; FirstLine = 556
; Folding = -----
; EnableXP