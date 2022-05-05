Imports System
Imports Microsoft

Module Program
    Dim Userin As String
    Dim i As Integer
    Public varibs As Dictionary(Of String, Single) = New Dictionary(Of String, Single)



    Sub Main(args As String())
        i = 0
        Console.Title = "EtaluCalc"
        Console.WriteLine("Welcome to EtaluCalc")
        ' one time message at beginning
        While True
            Console.WriteLine("What do you want?")
            Console.WriteLine("1 : help")
            Console.WriteLine("2 : expression")
            Console.WriteLine("3 : simple graph")
            Console.WriteLine("4 : quit")
            'main menu
            Userin = Console.ReadLine

            If Userin = "1" Then
                Call HelpPage()
            ElseIf Userin = "2" Then
                Call Expressionist()
            ElseIf Userin = "3" Then
                Call Grapher()
            ElseIf Userin = "4" Then
                Exit Sub
            Else
                Console.WriteLine("invalid input")
            End If
            ' take input and go to according screen
        End While
    End Sub

    Sub Expressionist()
        varibs.Clear()
        Console.WriteLine("Please input expression.")
        i = 0
        Console.WriteLine("out : " & expressioner(Console.ReadLine, "X", "Y"))
    End Sub

    Function expressioner(InnyPut As String, X As String, Y As String)
        Dim Expression() As String
        Dim Expressor As String
        Dim k As Integer
        Dim l As Integer
        Expressor = InnyPut
        Expression = Split(Expressor, " ")

        While True
            If IsNumeric(Expression(0)) Then
                Return Expression(0)
            Else
                For j = 0 To Expression.Length - 3

                    If IsOpp(Expression(j)) Then
                        k = 1
                        While Expression(j + k) = ""
                            k += 1
                        End While
                        l = k + 1
                        While Expression(j + l) = ""
                            l += 1
                        End While
                        If Not (IsOpp(Expression(j + k)) Or IsOpp(Expression(j + l))) Then
                            If Expression(j + k) = "X" Or Expression(j + k) = "x" Then
                                Expression(j + k) = X
                            ElseIf Expression(j + k) = "Y" Or Expression(j + k) = "y" Then
                                Expression(j + k) = Y
                            End If
                            If Expression(j + l) = "X" Or Expression(j + l) = "x" Then
                                Expression(j + l) = X
                            ElseIf Expression(j + l) = "Y" Or Expression(j + l) = "y" Then
                                Expression(j + l) = Y
                            End If
                            'Console.WriteLine(Expression(j) & " " & Expression(j + k) & " " & Expression(j + l))
                            Expression(j) = functioner(Expression(j), Expression(j + k), Expression(j + l))
                            Expression(j + k) = ""
                            Expression(j + l) = ""
                            j = Expression.Length
                        End If
                    End If
                Next
            End If
        End While
    End Function

    Function functioner(opp As String, vara As String, varb As String)
        Dim snga As Single
        Dim sngb As Single
        If Not IsNumeric(vara) Then
            If Not varibs.ContainsKey(vara) Then
                Console.WriteLine("What is the value of " & vara & "?")
                varibs.Add(vara, CSng(Console.ReadLine))
            End If
            snga = varibs(vara)
        Else
            snga = CSng(vara)
        End If
        If Not IsNumeric(varb) Then
            If Not varibs.ContainsKey(varb) Then
                Console.WriteLine("What is the value of " & varb & "?")
                varibs.Add(varb, CSng(Console.ReadLine))
            End If
            sngb = varibs(varb)
        Else
            sngb = varb
        End If

        If opp = "+" Then
            Return snga + sngb
        ElseIf opp = "-" Then
            Return snga - sngb
        ElseIf opp = "*" Then
            Return snga * sngb
        ElseIf opp = "/" Then
            Return snga / sngb
        ElseIf opp = "^" Then
            Return snga ^ sngb
        ElseIf opp = "L" Then
            Return Math.Log(snga) / Math.Log(sngb)
        End If
        Return 0
    End Function

    Function IsOpp(charr As String)
        If charr = "+" Or charr = "-" Or charr = "*" Or charr = "/" Or charr = "^" Or charr = "L" Then
            Return True
        Else
            Return False
        End If
    End Function

    Sub Grapher()
        'Console.WriteLine("This has not been implemented yet :/")
        'Exit Sub
        Dim Width As Integer = 21
        Dim Height As Integer = 21
        Dim Temp As String
        Dim Grid(Width + 1, Height + 1) As Single
        Dim OutGrid(Width, Height) As String
        ' grid settings, I don't know if you'll be able to change them and have them still work properly, as I just want a 16x16 grid
        Console.WriteLine("Please input expression.")
        Userin = Console.ReadLine
        For i = 0 To Width + 1
            For j = 0 To Height + 1
                Grid(i, j) = expressioner(Userin, CStr(i - Width / 2), CStr(j - Width / 2))
            Next
        Next
        'For j = 0 To Height
        'Console.WriteLine(Grid(0, Height - j) & Grid(1, Height - j) & Grid(2, Height - j) & Grid(3, Height - j) &
        '                    Grid(4, Height - j) & Grid(5, Height - j) & Grid(6, Height - j) & Grid(7, Height - j) &
        '                     Grid(8, Height - j) & Grid(9, Height - j) & Grid(10, Height - j) & Grid(11, Height - j) &
        '                      Grid(12, Height - j) & Grid(13, Height - j) & Grid(14, Height - j) & Grid(15, Height - j))
        ' continue here, do logic for making plot
        'Next
        For i = 1 To Width
            For j = 1 To Height
                'Debug.Print(CStr(Grid(i, j)) & CStr(IsNeg(Grid(i, j))) & CStr(Grid(i + 1, j)) & CStr(IsNeg(Grid(i + 1, j))) & CStr(IsNeg(Grid(i, j + 1))) & CStr(IsNeg(Grid(i + 1, j + 1))))
                If (IsNeg(Grid(i - 1, j - 1)) Or IsNeg(Grid(i, j - 1)) Or IsNeg(Grid(i, j)) Or IsNeg(Grid(i, j))) And (Not (IsNeg(Grid(i - 1, j - 1)) And
                    IsNeg(Grid(i, j - 1)) And IsNeg(Grid(i - 1, j)) And IsNeg(Grid(i, j)))) Then
                    OutGrid(i, j) = "#"
                Else
                    OutGrid(i, j) = " "
                End If
            Next
        Next
        For j = 1 To Height
            Temp = ""
            For i = 1 To Width
                Temp = Temp & OutGrid(i, Height - j) & OutGrid(i, Height - j)
            Next
            Console.WriteLine(Temp)
        Next
    End Sub

    Function IsNeg(Num As Single)
        Return Num < 0
    End Function

    Sub HelpPage()
        Console.WriteLine(vbCrLf & "Welcome to EtaluCalc," & vbCrLf & vbCrLf &
                          "This is a calculation program I have made that uses a custom notation." & vbCrLf &
                          "Since you are in the help page, I assume you do not know how the notation works." & vbCrLf &
                          "It's quite easy once you understand it: it's not like standard notation, with it" & vbCrLf &
                          "going variable, operator, variable; this one goes operator, variable, variable." & vbCrLf &
                          "For example, '+ 80 15' (spaces needed) means '80+15,' which then equals 95." & vbCrLf & vbCrLf &
                          "'How do you do multiple operations' I hear you ask? well, that is easy(ish)." & vbCrLf &
                          "It's easy in concept, but may be hard to get your head around." & vbCrLf &
                          "You see, to put in a second opperation, you replace where a variable would go" & vbCrLf &
                          "with an operator and place the next variables behind that operator." & vbCrLf &
                          "New example, '+ - 80 5 15' means '80-5+15,' which equals 90." & vbCrLf & vbCrLf &
                          "If you put your own variable, like 'A,' then you will be asked what it equals." & vbCrLf &
                          "There are a few opperations in here. They might not be as many as you may want" & vbCrLf &
                          "but they are enough for me." & vbCrLf &
                          "'+ X Y' : X + Y" & vbCrLf &
                          "'- X Y' : X - Y" & vbCrLf &
                          "'* X Y' : X * Y" & vbCrLf &
                          "'/ X Y' : X / Y" & vbCrLf &
                          "'^ X Y' : X ^ Y" & vbCrLf &
                          "'L X Y' : log_Y(X)" & vbCrLf & vbCrLf &
                          "Examples :" & vbCrLf & vbCrLf &
                          "In expression, try the quadratic formula :" & vbCrLf &
                          " / + - 0 B ^ - ^ B 2 * * 4 A C / 1 2 * 2 A" & vbCrLf &
                          "In simple graph, try drawing a circle with :" & vbCrLf &
                          " - 0 - + ^ Y 2 ^ X 2 ^ Radius 2" & vbCrLf & vbCrLf &
                          "That's all I have to say, hope you enjoy" & vbCrLf)
    End Sub
End Module
