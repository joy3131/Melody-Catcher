
;-----------------;
;JoyCode_코드간격비교; 
;-----------------;

Global cntidn.w = 0
Global cntdif.w = 0
Global lastA.w = 0
Global lastB.w = 0
Global last.w = 0
Global j.w
Global l.w
Global score.f
Global Dim CodeArray1.s(10)
Global Dim CodeArray2.s(10)



Structure TwoNotes
   N.i
   FirstNote.s
   SecondNote.s
EndStructure

Procedure.i SubNote_Init()
  Global Dim TwoNotes.TwoNotes(144)
  For N=0 To 143
    Read.s Firstnote.s
    Read.s SecondNote.s
    TwoNotes(N)\FirstNote = FirstNote
    TwoNotes(N)\SecondNote = SecondNote
  Next
  
  Global Dim g_Sub.w(11)
  For i=0 To 11
    Read.w Subtract.w
    g_Sub(i)=Subtract.w
  Next

EndProcedure

Procedure.w MatchTwoArray(lValue)
  Define Num.w
  If 0<=lValue And lValue<11
    Num = 0
  Else 
    If 11<=lValue And lValue<23
      Num = 1
    Else
      If 23<=lValue And lValue<35
        Num = 2
      Else
        If 35<=lValue And lValue<47
          Num = 3
        Else
          If 47<=lValue And lValue<59
            Num = 4
          Else
            If 59<=lValue And lValue<71
              Num = 5
            Else
              If 71<=lValue And lValue<83
                Num = 6
              Else
                If 83<=lValue And lValue<95
                  Num = 7
                Else
                  If 95<=lValue And lValue<107
                    Num = 8
                  Else
                    If 107<=lValue And lValue<119
                      Num = 9
                    Else
                      If 119<=lValue And lValue<131
                        Num = 10
                      Else
                        If 131<=lValue And lValue<143
                          Num = 11
                 
                        EndIf
                      EndIf
                    EndIf
                  EndIf
                EndIf     
              EndIf
            EndIf
          EndIf
        EndIf
      EndIf
    EndIf
  EndIf


  ProcedureReturn g_Sub(Num)
 EndProcedure
 
 SubNote_Init()
 
 Procedure FindTwoNote(Value1.s,Value2.s)
   For N=0 To 143
      If Value1=TwoNotes(N)\FirstNote And Value2 = TwoNotes(N)\SecondNote
        ProcedureReturn N
      EndIf
    Next
    ProcedureReturn -1
  EndProcedure
  
  Procedure Compare_Interval(last, Array CodeArray1.s(1), Array CodeArray2.s(1),l)
    cntidn = 0
    cntdff = 0
    score = 0
    If l=last-1
      SetGadgetText(3, "Most likely different")
    Else
      
      For j = l To last-1
    If MatchTwoArray(FindTwoNote(CodeArray1(j),CodeArray1(j+1))) = MatchTwoArray(FindTwoNote(CodeArray2(j),CodeArray2(j+1)))
      cntidn = cntidn +1
      
      For k.w = j+1 To last-1
        If MatchTwoArray(FindTwoNote(CodeArray1(k),CodeArray1(k+1))) = MatchTwoArray(FindTwoNote(CodeArray2(k),CodeArray2(k+1)))
          cntidn = cntidn+1
        Else
          cntdif = cntdif+1
        EndIf
      Next  
      Break
     
    EndIf
    Next
    
    ;Break 다음
    score = (last-j)/cntdif
    ;비교한 코드의 2/3이상 같으면 같다고 표시
   If score <3
     Compare_Interval(last,CodeArray1(),CodeArray2(),l+1)
      
    Else
      SetGadgetText(3, "Most likely identical")
    EndIf
  EndIf
  
EndProcedure


OpenWindow  (0,0,0,360,110,"Compare The Notes", #PB_Window_WindowCentered | #PB_Window_ScreenCentered | #PB_Window_BorderLess)
ButtonGadget(0, 10, 10,340, 24, "파일 선택")
ButtonGadget(1, 10, 44,340, 24, "My Recording")
ButtonGadget(2, 55, 78, 70, 24, "Compare")
TextGadget  (3,135, 84,300, 20, "")
;ButtonGadget(4, 10, 78, 20, 24, "?", #PB_Text_Center)
ButtonGadget(5, 10, 78, 40, 24, "Quit", #PB_Text_Center)
LoadFont    (0, "Arial", 11)
For i = 0 To 5 : SetGadgetFont(i,FontID(0)) : Next i
AddKeyboardShortcut(0, #PB_Shortcut_Command | #PB_Shortcut_Q, #PB_Event_CloseWindow)

 



;A.s = CodeArray(4)
;B.s = CodeArray(5)
;C.s = CodeArray(24)
;D.s = CodeArray(50)


;;;;;;;;;;;;;;
UseMD5Fingerprint()
;;;;;;;;;;;;;;;

Repeat
 Event = WaitWindowEvent(100)
 Select Event
  Case #PB_Event_Gadget
   Gadget = EventGadget()
  Select Gadget
    Case 0
      temp$ = OpenFileRequester("Choose file", "", "Text(*.txt)|*.txt", 0)
    If temp$ <> ""
     SetGadgetText(Gadget, GetFilePart(temp$));+ " - " + StrF(FileSize(temp$) / 1024, 3) + "kb - " + FormatDate("%yyyy-%mm-%dd %hh:%ii:%ss", GetFileDate(Temp$, #PB_Date_Modified)))
      ;If Gadget = 0
     MD5a$ = FileFingerprint(temp$, #PB_Cipher_MD5)
     fileA$ = temp$
     
     If ReadFile(0,fileA$)
       ReDim CodeArray1(Lof(0)/4)
       test.s = ReadString(0)
       lastA = Lof(0)

        If Mid(test,3,2) = "C#" Or Mid(test,3,2) = "D#" Or Mid(test,3,2) = "F#" Or Mid(test,3,2) = "G#" Or Mid(test,3,2) = "A#"
         CodeArray1(0) = Mid(test,3,2)
         test = RemoveString(test ,Left(test,7) , #PB_String_NoCase , 1, 1) 
       Else
         CodeArray1(0) = Mid(test,3,2)
         test = RemoveString(test ,Left(test,6) , #PB_String_NoCase , 1, 1) 
       EndIf
       
       ;Debug CodeArray1(0)
       
   
     
       For i=1 To lastA/4
         If Left(test,2) = "C#" Or Left(test, 2) = "D#" Or Left(test, 2) = "F#" Or Left(test,2) = "G#" Or Left(test,2) = "A#"
           CodeArray1(i) = Left(test,2)
           ;Debug CodeArray1(i)
           test = RemoveString(test , "#", #PB_String_NoCase , 1, 1) 
           test = RemoveString(test ,Left(test,4) , #PB_String_NoCase , 1, 1) 
    
         Else
           CodeArray1(i) = Left(test,1)
           ;Debug CodeArray1(i)
           test = RemoveString(test ,Left(test,4) , #PB_String_NoCase , 1, 1) 
        
         EndIf
     Next
     
     
        CloseFile(0)
Else 
  MessageRequester("Information","Couldn’t open the file!") 
EndIf
EndIf


Case 1
     SetGadgetText(Gadget, "My Recording" )
                  
      If ReadFile(0,"joy.txt")
       ReDim CodeArray2.s(Lof(0)/4)
       test2.s = ReadString(0)
       lastB = Lof(0)
       
       
       If Mid(test2,3,2) = "C#" Or Mid(test2,3,2) = "D#" Or Mid(test2,3,2) = "F#" Or Mid(test2,3,2) = "G#" Or Mid(test2,3,2) = "A#"
         CodeArray2(0) = Mid(test2,3,2)
          test2 = RemoveString(test2 ,Left(test2,7) , #PB_String_NoCase , 1, 1)
       Else
         CodeArray2(0) = Mid(test2,3,1)
         test2 = RemoveString(test2 ,Left(test2,6) , #PB_String_NoCase , 1, 1) 
       EndIf
       
       ;Debug CodeArray2(0)
       
     
     
       For i=1 To lastB/4
         If Left(test2,2) = "C#" Or Left(test2, 2) = "D#" Or Left(test2, 2) = "F#" Or Left(test2,2) = "G#" Or Left(test2,2) = "A#"
           CodeArray2(i) = Left(test2,2)
           ;Debug CodeArray2(i)
           test2 = RemoveString(test2 , "#", #PB_String_NoCase , 1, 1) 
           test2 = RemoveString(test2 ,Left(test2,4) , #PB_String_NoCase , 1, 1) 
    
         Else
           CodeArray2(i) = Left(test2,1)
           ;Debug CodeArray2(i)
           test2 = RemoveString(test2 ,Left(test2,4) , #PB_String_NoCase , 1, 1) 
        
         EndIf
     Next
     
     
     
        CloseFile(0)
Else 
  MessageRequester("Information","Couldn’t open the file!") 
EndIf
;SetGadgetText(Gadget,CodeArray2(20)+","+CodeArray2(21)+","+MatchTwoArray(FindTwoNote(CodeArray2(20),CodeArray2(21))))
     
     GadgetToolTip(Gadget, Temp$)
   
  Case 2
   If MD5a$ = "" ;Or MD5b$ = ""
    MessageRequester("Information", "Please select file to compare.")
  Else
    last = 150
    If lastA<lastB And lastA/4 <150
      last = lastA/4
    Else 
      If lastB/4<150
      last = lastB/4
    EndIf
  EndIf 
  
    
    ;Debug CodeArray1(1)
    Compare_Interval(last,CodeArray1(),CodeArray2(),0)
    
    EndIf 
   ;Case 4 : MessageRequester("Information", "TinyCompare compares the MD5-fingerprints of two files. If the fingerprints are identical, the chance is high, that the two files are identical.")
  EndSelect
 EndSelect
Until Event = #PB_Event_CloseWindow Or Event = #PB_Event_Menu Or Gadget = 5






DataSection
  TwoNotes:
  ;1) (0,0,0)인경우 (12개)
  Data.s "C#","C#"
  Data.s "D#","D#"
  Data.s "E","E"
  Data.s "F","F"
  Data.s "F#","F#"
  Data.s "G#","G#"
  Data.s "A#","A#"
  Data.s "B","B"
  Data.s "C","C"
  Data.s "D","D"
  Data.s "G","G"
  Data.s "A","A"
  
  ;2) (1,1,1)인 경우(12개)
  Data.s "D#","E"
  Data.s "E","F"
  Data.s "F","F#" 
  Data.s "A#","B"
  Data.s "C","C#"
  Data.s "D","D#" 
  Data.s "F#","G"
  Data.s "G#","A"
  Data.s "G","G#"
  Data.s "C#","D"
  Data.s "A","A#"
  Data.s "B","C"
  
  ;3) (2,2,2)인 경우(12개)
  Data.s "C#","D#" 
  Data.s "D#","F"
  Data.s "E","F#"
  Data.s "F#","G#"
  Data.s "G#","A#"
  Data.s "B","C#"
  Data.s "D","E"
  Data.s "F","G"
  Data.s "A","B"
  Data.s "A#","C"
  Data.s "C","D" 
  Data.s "G","A"
  
  ;4) (3,3,3)인 경우(12개)
  Data.s "C#","E"
  Data.s "D#","F#"
  Data.s "F","G#"
  Data.s "G#","B"
  Data.s "A#","C#"
  Data.s "C","D#"
  Data.s "D","F"
  Data.s "E","G"
  Data.s "F#","A"
  Data.s "G","A#"
  Data.s "B","D"
  Data.s "A","C"

  ;5) (4,4,4)인 경우(12개)
  Data.s "C#","F"
  Data.s "E","G#"
  Data.s "F#","A#"
  Data.s "B","D#"
  Data.s "C","E"
  Data.s "D","F#"
  Data.s "D#","G"
  Data.s "F","A"
  Data.s "A","C#"
  Data.s "G","B"
  Data.s "G#","C"
  Data.s "A#","D"
  
  ;6) (5,5,5)인 경우(12개)
  Data.s "C#","F#"
  Data.s "D#","G#"
  Data.s "F","A#"
  Data.s "F#","B"
  Data.s "G#","C#"
  Data.s "A#","D#"
  Data.s "B","E"
  Data.s "C","F"
  Data.s "E","A"
  Data.s "G","C"
  Data.s "A","D"
  Data.s "D","G"
  
  ;7) (6,6,6)인 경우(12개)
  Data.s "E","A#"
  Data.s "F","B"
  Data.s "A#","E"
  Data.s "B","F"
  Data.s "G","C#"     
  Data.s "A","D#"
  Data.s "F#","C"
  Data.s "G#","D"
  Data.s "C","F#" 
  Data.s "D","G#"
  Data.s "D#","A"
  Data.s "C#","G"
  
  ;8) (7,7,7)인 경우(12개)
  Data.s "C#","G#"
  Data.s "D#","A#"
  Data.s "E","B"
  Data.s "F#","C#"
  Data.s "G#","D#"
  Data.s "A#","F"
  Data.s "B","F#"
  Data.s "A","E"
  Data.s "F","C"
  Data.s "C","G"
  Data.s "D","A"
  Data.s "G","D"
  
  ;9) (8,8,8)인 경우(12개)
  Data.s "D#","B"
  Data.s "F","C#"
  Data.s "G#","E"
  Data.s "A#","F#"
  Data.s "G","D#"
  Data.s "A","F"
  Data.s "E","C"
  Data.s "F#","D"
  Data.s "C","G#"
  Data.s "C#","A"
  Data.s "D","A#"
  Data.s "B","G"
  
  ;10) (9,9,9)인 경우(12개)
  Data.s "C#","A#"
  Data.s "E","C#"
  Data.s "F#","D#"
  Data.s "G#","F"
  Data.s "B","G#"
  Data.s "G","E"
  Data.s "A","F#"
  Data.s "D#","C"
  Data.s "F","D"
  Data.s "D","B"
  Data.s "A#","G"
  Data.s "C","A"
  
  ;11) (10,10,10)인 경우(12개)
  Data.s "C#","B"
  Data.s "D#","C#"
  Data.s "F","D#"
  Data.s "F#","E"
  Data.s "G#","F#"
  Data.s "A#","G#"
  Data.s "G","F"
  Data.s "E","D"
  Data.s "C","A#"
  Data.s "B","A"
  Data.s "D","C"
  Data.s "A","G"
  
  ;12) (11,11,11)인 경우(12개)
  Data.s "E","D#"
  Data.s "F","E"
  Data.s "F#","F"
  Data.s "B","A#"
  Data.s "G","F#"
  Data.s "A","G#"
  Data.s "D#","D"
  Data.s "C#","C"
  Data.s "C","B"
  Data.s "D","C#"
  Data.s "G#","G"
  Data.s "A#","A"
  
 
  
  
     Subtract:
   Data.w 0 
   Data.w 1
   Data.w 2
   Data.w 3
   Data.w 4
   Data.w 5
   Data.w 6
   Data.w 7
   Data.w 8
   Data.w 9
   Data.w 10
   Data.w 11

 EndDataSection
 
; IDE Options = PureBasic 5.60 (Windows - x86)
; CursorPosition = 486
; FirstLine = 442
; Folding = -
; EnableXP