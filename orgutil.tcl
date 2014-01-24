#! /usr/bin/tclsh
########################################################################
#
# Copyright © 2011, Patrick M Brennan.
#
# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# The complete text of the license can be found at:
# http://www.gnu.org/licenses/gpl.txt
#
########################################################################
# Version 2011-02-07-01
# supercedes:
# Version 2011-01-23-1
########################################################################
#
# orgutil.tcl
#
# Tools for converting files into and out of Org-mode, and for performing
# various analyses and conversions.
#
# The primary use case for this utility is to convert between Org-mode outline
# text and AbstractSpoon's ToDoList. See these file formats documented
# respectively here:
#
# Org Mode: http://orgmode.org/
#
# ToDoList: http://www.abstractspoon.com/
#           http://www.codeproject.com/KB/applications/todolist2.aspx
#
# This code has a lot of utility functions which may readily be repurposed for
# almost anything having to do with filtering or examining Org-mode or TDL
# files. Additionally, it should not prove difficult to implement additional
# converters for other file types.
#
# Use the -tdl2org option to convert files from TDL (XML) to Org-mode.
#
# Use the -org2tdl option to convert files from Org-mode to TDL.
#
# Use the -done option to generate a report of items which have been completed
# and marked DONE in the given file.
# 
# Use the -test option to test the routines in this file. This will help guide
# any development you may wish to pursue with this work (particularly, it is
# hoped, by making it difficult to break existing functionality.)
#
# WORKING MODULES:
# Org -> [internal]
# [internal] -> Org
# TDL (XML) -> Org
# Org -> TDL (XML)
#
# PLANNED:
# 
#

########################################################################
#
# parseLineIntoHeadlineAndTags :private:
#
# Given a line containing a preamble, some text, and possibly some tags,
# return the headline alone and the tags list
#
proc parseLineIntoHeadlineAndTags {line preamble} {
    set tagsList {}
    set headline {}

    set preambleLen [string length $preamble]

    if {$preambleLen == 0} {
        set headlineAndTags $line
    } else {
        set headlineAndTags [string range $line $preambleLen end]
    }

    # Extract the tags
    # Tags are normal words containing letters, numbers, `_', and `@'
    # So the regexp to find them would be:
    # :[:a-zA-Z0-9_@]*:
    
    set allTags {}
    regexp {:[:a-zA-Z0-9_@]*:} $headlineAndTags allTags

    # TEMPORARY
    # Also pull out / and - and normalize them.
    #regexp {:[:a-zA-Z0-9_@\-\/]*:} $headlineAndTags allTags
    #set allTags [regsub -all {[\-\/]} $allTags "_" ]
    
    if {[string length $allTags] > 0} {
        
        set break [string first $allTags $headlineAndTags]

        set headline [string trim [string range $headlineAndTags 0 [expr {$break-1}]]]
        #set headline [string range $headlineAndTags 0 [expr {$break-1}]]

        set tagsList [split [string trim $allTags :] :]
    } else {
        set allTags {}

        set headline [string trim $headlineAndTags]
        #set headline $headlineAndTags

        set tagsList {}
    }

    return [list $headline $tagsList]
}

########################################################################
# 
# extractPropertiesFromBody :private:
#
# Grab the properties block from the body
# return [newbody properties]
#
# Properties should not be one of (as these are reserved by Org):
#      TODO         The TODO keyword of the entry.
#      TAGS         The tags defined directly in the headline.
#      ALLTAGS      All tags, including inherited ones.
#      CATEGORY     The category of an entry.
#      PRIORITY     The priority of the entry, a string with a single letter.
#      DEADLINE     The deadline time string, without the angular brackets.
#      SCHEDULED    The scheduling timestamp, without the angular brackets.
#      CLOSED       When was this entry closed?
#      TIMESTAMP    The first keyword-less timestamp in the entry.
#      TIMESTAMP_IA The first inactive timestamp in the entry.
#      CLOCKSUM     The sum of CLOCK intervals in the subtree.  org-clock-sum
#                   must be run first to compute the values.
#      BLOCKED      "t" if task is currently blocked by children or siblings
#      ITEM         The content of the entry.
proc extractPropertiesFromBody {body} {
    set properties {}
    set newBody $body

    set foundBegin -1
    set foundEnd -1

    # Try to find the beginning and ending of the properties
    # block
    set lineNum 0
    foreach line $body {
        if {($foundBegin == -1)&&([string trim $line] == ":PROPERTIES:")} {
            set foundBegin $lineNum
        } elseif {($foundEnd == -1)&&([string trim $line] == ":END:")} {
            set foundEnd $lineNum
        }
        incr lineNum
    }

    if {($foundBegin != -1)&&($foundEnd != -1)&& \
            ($foundEnd > $foundBegin)} {
        set propertyLines [lrange $body $foundBegin $foundEnd]
        set newBody [lreplace $body $foundBegin $foundEnd]

        # Now figure out the properties in propertyLines
        foreach line $propertyLines {
            set trimmedLine [string trim $line]
            if {($trimmedLine == ":PROPERTIES:") || \
                    ($trimmedLine == ":END") || \
                    ($trimmedLine == "")} {
                continue
            }

            regexp {^\s*:([a-zA-Z0-9_@]*):\s*(.*)} $trimmedLine \
                matched propName propValue
            set propValue [string trim $propValue]
            if {($propName != "")&&($propValue != "")} {
                lappend properties $propName $propValue
            }
        }
    }

    return [list $newBody $properties]
}

########################################################################
#
# parseLinesIntoOrg :private:
#
# Parse a buffer of text lines into org items.
# Each org item is a list composed of:
#
# {headline tags properties body children}
#
proc parseLinesIntoOrg {linebuf level} {

    #puts "DBG: parseLinesIntoOrg: level = $level"

    if {$level == 0} {
        set preamble ""
    } else {
        set preamble "[string repeat * $level] "
    }

    set nextlevel [expr {$level+1}]
    set nextpreamble "[string repeat * $nextlevel] "

    #puts "DBG: nextpreamble='$nextpreamble'"

    set breakingLines {}

    # Do we have a case of empty headline?
    set emptyHeadline 0
    if {($level == 0) && ([string index [lindex $linebuf 0] 0] == {*})} {
        set emptyHeadline 1
    } elseif {($level > 0) && \
                  ([string first $preamble [lindex $linebuf 0]] != 0)} {
        set emptyHeadline 1
    }

    #puts "DBG: emptyHeadline=$emptyHeadline"

    if {$emptyHeadline} {
        set searchForNextLevelFrom 0
    } else {
        set searchForNextLevelFrom 1
    }

    # Find the first instance of any next level below this one
    set nLines [llength $linebuf]
    for {set line $searchForNextLevelFrom} {$line < $nLines} {incr line} {
        if {[regexp {^\*+ } [lindex $linebuf $line]]} {
            break
        }
    }
    
    if {$line < $nLines} {

        lappend breakingLines $line

        # Find all the remaining instances of the next level
        for {incr line} {$line < $nLines} {incr line} {
            if {[string first $nextpreamble [lindex $linebuf $line]] == 0} {
                lappend breakingLines $line
            }
        }
    }

    # Now we have:
    # linebuf[0] containing the headline and tags for this level
    # a list breakingLines {x y z ... } containing the line #s for the 
    #        child levels of this level
    # potentially lines 1-x containing additional text for this level

    if {$emptyHeadline == 0} {
        foreach {headline tags} \
            [parseLineIntoHeadlineAndTags [lindex $linebuf 0] $preamble] {}
    } else {
        set headline {}
        set tags {}
    }

    set properties {}

    # Parse and store the body text.
    set body {}
    set firstLineOfBody 1
    if {[llength $breakingLines] > 0} {
        set lastLineOfBody [expr {[lindex $breakingLines 0] - 1}]
    } else {
        set lastLineOfBody [expr {[llength $linebuf] - 1}]
    }

    if {$lastLineOfBody >= $firstLineOfBody} {
        for {set index $firstLineOfBody} \
            {$index <= $lastLineOfBody} \
            {incr index} {
                set line [lindex $linebuf $index]
                lappend body $line
            }
    }

    foreach {body properties} [extractPropertiesFromBody $body] {}

    set children {}
    set nChildren [llength $breakingLines]
    set lastIndex [expr {$nChildren-1}]
    for {set index 0} {$index < $nChildren} {incr index} {
        set fromLine [lindex $breakingLines $index]
        if {$index < $lastIndex} {
            set toLine [lindex $breakingLines [expr {$index+1}]]
            incr toLine -1
        } else {
            set toLine end
        }

        set childlinebuf [lrange $linebuf $fromLine $toLine]
        lappend children [parseLinesIntoOrg $childlinebuf $nextlevel]
    }

    return [list $level $headline $tags $properties $body $children]
}

########################################################################
#
# parseTextIntoOrg :public:
#
# Parse a buffer of text into an org outline tree
#
# An org tree at level N consists of the following:
# * _at the beginning of a line only_, N stars plus a space, e.g.
#   "*** " at level 3
#   (level 0 has no space)
# * Headline
# * Tags
# * Properties
# * Body
# * Children
# 
# and will be returned as a TCL list in the following form:
# {level headline tags properties body children}
#
# where:
# level is a number
# headline is the node's headline
# tags is a list of tags appropriate to this node (and its children)
# properties is a list of key/value pairs
# body is a list of lines of the body text of the node
# children is a list of the child nodes of this node
#
proc parseTextIntoOrg {buf {level 0}} {
    set docHeader ""
    set docText ""
    set gotDocHeaderAndText 0
    
    set buf [split $buf "\n"]
    return [parseLinesIntoOrg $buf $level]
}

########################################################################
# 
# parseOrgFile :public:
#
# Read in the file named, and parse it as an org file.
#
# return the org tree as a TCL list.
#
proc parseOrgFile {filename} {
    if {$filename == ""} {
        set chan stdin
    } else {
        set chan [open $filename]
    }
    set rv [parseTextIntoOrg [read $chan]]
    
    if {$filename != ""} {
        close $chan
    }
    return $rv
}

########################################################################
#
# formatFirstLine
#
# Given a level, a headline, and a tags list, write the first line
# of a level entry.
#
proc formatFirstLine {level headline tagsList {justify 1}} {
    # The preamble e.g. *** 
    set preamble [string repeat "*" $level]
    if {$level > 0} {
        append preamble " "
    }

    # The tags
    if {$tagsList != {}} {
        set tags ":"
        foreach tag $tagsList {append tags "${tag}:"}
    } else {
        set tags ""
    }

    set firstLine ""
    set nonEmptyFirstLine [expr {($headline != "")||($tags != "")}]

    if {$tags == ""} {
        set firstLine "$preamble$headline"
    } elseif {$nonEmptyFirstLine} {
        if {$justify} {
            set nspaces \
                [expr {77 - [string length $preamble] \
                           - [string length $tags] \
                           - [string length $headline]}]
            if {$nspaces < 1} {
                set firstLine "$preamble$headline $tags"
            } else {
                set spaces [string repeat { } $nspaces]
                set firstLine "$preamble$headline$spaces$tags"
            } 
        } else {
            set firstLine "$preamble$headline $tags"
        }
    } else {
        set firstLine {}
    }
    return $firstLine
}

########################################################################
#
# writeOrgPropertiesIntoLineBuffer :private:
#
proc writeOrgPropertiesIntoLineBuffer {propsList level textLinesName} {
    upvar $textLinesName textLines

    if {$propsList != {}} {
        if {$level == 0} {
            set indentation ""
        } else {
            set indentation [string repeat " " $level]
            set indentation "$indentation "
        }
        
        lappend textLines "${indentation}:PROPERTIES:"
        foreach {key value} $propsList {
            lappend textLines "${indentation}:${key}: $value"
        }
        lappend textLines "${indentation}:END:"
    }
}

########################################################################
#
# writeOrgIntoLines :private:
#
# Given an org tree in the following form:
# {level headline tags body children}
# Write out a list of lines corresponding to the text.
#
proc writeOrgIntoLines {orgTree} {
    #puts stderr "writeOrgIntoLines..."
    foreach {level headline tagsList propsList body children} $orgTree {}

    # A list of the lines of text to write out
    set textLines {}

    if {($headline != "")||($tagsList != {})} {
        lappend textLines [formatFirstLine $level $headline $tagsList]
    }

    # Add any properties
    writeOrgPropertiesIntoLineBuffer $propsList $level textLines

    foreach bodyLine $body {
        lappend textLines $bodyLine
    }
    
    foreach child $children {
        set childList [writeOrgIntoLines $child]
        foreach line $childList {
            lappend textLines $line
        }
    }

    return $textLines
}

########################################################################
#
# lineBufferToText :private:
#
proc lineBufferToText {lines} {
    set rv ""
    set nLines [llength $lines]
    set lineNum 1
    foreach line $lines {
        append rv $line
        if {$lineNum < $nLines} {
            append rv "\n"
        }
        incr lineNum
    }
    return $rv
}

########################################################################
#
# normalizeAllTags :public:
#
# traverse an org tree and normalize all the tags.
#
proc normalizeAllTags {orgTree} {
    foreach {level headline tagsList propsList body children} $orgTree {}

    set newTags [normalizeTags $tagsList]
    
    set newChildren {}
    foreach child $children {
        lappend newChildren [normalizeAllTags $child]
    }

    return [list $level $headline $newTags $propsList $body $newChildren]
}

########################################################################
#
# writeOrgIntoText :private:
#
proc writeOrgIntoText {orgTree} {
    return [lineBufferToText [writeOrgIntoLines $orgTree]]
}

########################################################################
#
# writeOrgFile :private:
#
proc writeOrgFile {orgTree filename} {
    if {$filename == ""} {
        set chan stdout
    } else {
        set chan [open $filename "w"]
    }
    puts $chan [writeOrgIntoText $orgTree]
    if {$filename != ""} {
        close $chan
    }
}

########################################################################
#
# eligibleForDoneList
#
# 
proc eligibleForDoneList {date
                          dated undated
                          startDate endDate} {

    # Are we looking for everything?
    if {$dated && $undated} {
        return 1 ; # Yeah, we're looking for everything.
    }

    # Are we looking for undated items, and is it undated?
    if {$undated && $date == ""} {
        return 1
    }

    # Are we looking for dated items, and is it dated?
    if {$dated == 1 && $date != ""} {

        # Are we looking for ANY date?
        if {$startDate == -1 && $endDate == -1} {
            return 1
        }

        set dateAsExcel [dateStringToExcelSerialDate $date]

        if {$startDate == -1 || $dateAsExcel >= $startDate} {
            if {$endDate == -1 || $dateAsExcel <= $endDate} {
                return 1
            }
        }
    }

    return 0
}
    

########################################################################
#
# getDoneHeadlinesFromOrgList
#
# Given a list of Org nodes, e.g. 
# { level headline tags properties body children }
#
# Return a list of all the headlines which are tagged as DONE.
#
# TODO: specify a range of dates to test the item against.
# options: {later than}-{earlier than}
#          {later than}-
#                      -{earlier than}
#          <undated>
#          <all with dates>
#          <all>
proc getDoneHeadlinesFromOrgList {orgList rv
                                  dated undated
                                  startDate endDate lineNumbers} {

    # Get the top-level task and see if it qualifies.
    foreach {level headline tags properties body children} $orgList {}

    if {[string first "DONE " $headline] == 0} {
        
        foreach {newbody dueDate doneDate} [extractDatesFromBody $body] {}
        
        # Check against criteria and add it to the list
        if {[eligibleForDoneList \
                 $doneDate $dated $undated $startDate $endDate]} {
            if {$doneDate == ""} {
                lappend rv $headline
            } else {
                lappend rv "($doneDate) $headline"
            }
        }
    }

    # Now traverse the child tasks and see if they qualify.
    foreach child $children {
        set rv [getDoneHeadlinesFromOrgList $child $rv $dated $undated \
               $startDate $endDate $lineNumbers]
    }

    return $rv
}

########################################################################
#
# getDoneHeadlines
#
# Given an Org-mode text buffer...
#
# Return a list of all the headlines which are tagged as DONE.
#
proc getDoneHeadlines {orgDoc} {
    set orgList [parseTextIntoOrg $orgDoc]
    return [getDoneHeadlinesFromOrgList $orgList {} 1 1 -1 -1 0]
}

########################################################################
#
# Parse TDL file into Org
#
# Tags are normal words containing letters, numbers, `_', and `@'
# 

package require xml
package require xml::tclparser

# This is an array whose indices will be constructed of
# tag,attribute pairs to indicate detected attributes in each
# read tag. The value set will be 1.
array unset tagAttributes

# This is a string which will contain the org-mode formatted
# output representing the found tasks
set orgout {}
set parseCategories 1 ; # 1=find categories in TDL list
set tdlDepth 0 ; # Level, used to track how deep in task tree we are.

# A list of text lines containing the output
set outputList {}

########################################################################
#
# normalizeToOrgTag :private:
#
# given a putative tag, make sure it conforms with the Org-mode tag spec.
#
# Org Mode tags are normal words containing letters, numbers, `_', and `@'
#
set goodTagChars \
    {abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789_@}
proc normalizeToOrgTag {cat} {
    global goodTagChars
    set rv ""
    set chars [split $cat {}]
    foreach char $chars {
        if {[string first $char $goodTagChars] != -1} {
            append rv $char
        } else {
            append rv "_"
        }
    }
    return $rv
}

########################################################################
#
# normalizeTags :private:
#
# given a list of tags in an item, make sure they are all legal Org-mode tags.
#
proc normalizeTags {tagsList} {
    set rv {}
    foreach tag $tagsList {
        lappend rv [normalizeToOrgTag $tag]
    }
    return $rv
}

proc handleCharacterData {data} {
    #puts "handleCharacterData: data=$data"
}

proc handleElementEnd {name args} {
    #puts "handleElementEnd: name=$name args=$args"
    global tdlDepth
    if {$name == "TASK"} {
        incr tdlDepth -1
    }
}

proc handleElementStart {name attlist args} {
    global tagAttributes orgout
    global outputList
    global parseCategories tdlDepth
    #puts "handleElementStart: name=$name attlist=$attlist args=$args"
    foreach {attName attValue} $attlist {
        #puts "        $attName : $attValue"
        set tagAttributes("$name,$attName") 1
    }

    if {($name == "CATEGORY")&&($parseCategories)} {
        # TODO: Gather up category names
    } elseif {$name == "TODOLIST"} {
        set headline ""
        foreach {attName attValue} $attlist {
            if {$attName == "PROJECTNAME"} {
                set headline $attValue
            }
        }
        if {$headline != ""} {
            lappend outputList $headline
        }
    } elseif {$name == "TASK"} {
        incr tdlDepth
        # Parse task into its components
        set headline {}
        set tagsList {}
        set bodyList {}
        set deadline {} ; # DEADLINE:
        set completiondate {} ; # CLOSED: 
        set percentdone 0 ; # percent complete: 100% == DONE
        set status {} ; # User-defined task status
        set fileref {} ; # reference to a file or uri.
        set personsList {} ; # List of Names of people assigned to this task
        set tdPriority "" ; # ToDoList priority (1-10)
        foreach {attName attValue} $attlist {
            if {[string first "CATEGORY" $attName] == 0} {
                lappend tagsList [normalizeToOrgTag $attValue]
            } elseif {$attName == "PERSON"} {
                set personsList [linsert $personsList 0 $attValue]
            } elseif {[string first "PERSON" $attName] == 0} {
                lappend personsList $attValue
            } elseif {$attName == "COMMENTS"} {
                set bodyList [split $attValue "\n"] 
            } elseif {$attName == "DUEDATE"} {
                # DUEDATE is a serial date number, convert it
                set deadline [excelSerialDateToDateString $attValue]
            } elseif {$attName == "DUEDATESTRING"} {
                # backup for DUEDATE, if it's not set
                set deadline [string range $attValue 0 9] ; # YYYY-MM-DD
            } elseif {$attName == "DONEDATE"} {
                # DONEDATE is a serial date number, convert it
                set completiondate [excelSerialDateToDateString $attValue]
            } elseif {$attName == "DONEDATESTRING"} {
                # backup for DONEDATE, if it's not set
                set completiondate [string range $attValue 0 9] ; # YYYY-MM-DD
            } elseif {$attName == "TITLE"} {
                set headline $attValue
            } elseif {$attName == "PERCENTDONE"} {
                set percentdone $attValue
            } elseif {$attName == "STATUS"} {
                set status $attValue
            } elseif {$attName == "FILEREFPATH"} {
                set fileref $attValue
            } elseif {$attName == "PRIORITY"} {
                set tdPriority $attValue
            }
        }

        ##################################################
        # We have all the information for the task now

        # task status: TODO or DONE ?
        if {($completiondate != "")} {
            # "DONE"
            if {[string first "DONE " $headline] != 0} {
                set headline "DONE $headline"
            }
        } else {
            # "TODO"
            if {[string first "TODO " $headline] != 0} {
                set headline "TODO $headline"
            }
        }
        set firstLine [formatFirstLine $tdlDepth $headline $tagsList 1]
        lappend outputList $firstLine

        #puts "headline=$headline"
        #puts "bodyList=$bodyList"
   
        set properties {}
        # handle personsList
        if {$personsList != {}} {
            set sPersons [join $personsList ","]
            lappend properties "TDL_PERSONS" $sPersons
        }

        # handle percentdone
        if {$percentdone > 0} {
            lappend properties "TDL_PERCENTDONE" $percentdone
        }

        # handle status
        if {$status != ""} {
            lappend properties "TDL_STATUS" $status
        }

        # handle fileref
        if {$fileref != ""} {
            lappend properties "TDL_FILEREF" $fileref
        }

        # handle tdPriority
        if {$tdPriority != ""} {
            lappend properties "TDL_PRIORITY" $tdPriority
        }

        # Compute Indentation using tdlDepth
        set indentation [string repeat " " $tdlDepth]
        set indentation "$indentation "
        
        # Deadline and closed date
        set deadlineTag ""
        set completeTag ""
        
        # handle deadline
        if {$deadline != ""} {
            set sDeadline [clock format [clock scan $deadline] \
                               -format "%Y-%m-%d %a"]
            set deadlineTag "DEADLINE: \<$sDeadline\>"
        }

        # handle completiondate
        if {$completiondate != ""} {
            set sDoneDate [clock format [clock scan $completiondate] \
                                    -format "%Y-%m-%d %a"]
            set completeTag "CLOSED: \[$sDoneDate\]"
        } 

        if {($deadlineTag != "") && ($completeTag != "")} {
            lappend outputList "${indentation}$deadlineTag $completeTag"
        } elseif {$deadlineTag != ""} {
            lappend outputList "${indentation}$deadlineTag"
        } elseif {$completeTag != ""} {
            lappend outputList "${indentation}$completeTag"
        }

        writeOrgPropertiesIntoLineBuffer $properties $tdlDepth outputList

        # indent the body text so as not to confuse Org-mode
        # if there happen to be special characters in the body.
        foreach bodyLine $bodyList {
            lappend outputList "${indentation}$bodyLine"
        }
    }
}

proc handleEndCDataSection {} {
}

set parser [xml::parser \
                -elementstartcommand handleElementStart \
                -characterdatacommand handleCharacterData \
                -elementendcommand handleElementEnd \
]

# We are expecting LFs in our comments; redefine what constitutes 
# whitespace so that they come through (otherwise they will all be
# converted to spaces).
set ::xml::Wsp { }

# parseTDLString :public:
# TODO: Add documentation.
proc parseTDLString {inString} {
    global parser outputList tdlDepth
    set tdlDepth 0
    set outputList {}
    $parser parse $inString
    return [lineBufferToText $outputList]
}

# parseTDLFile :public:
# TODO: Add documentation.
proc parseTDLFile {inFile} {
    if {$inFile == ""} {
        set inChan stdin
    } else {
        set inChan [open $inFile]
    }
    set inString [read $inChan]

    if {$inFile != ""} {
        close $inChan
    }

    return [parseTDLString $inString]
}

########################################################################
#
# DEVELOPMENT CODE
# Print out tag,attribute pairs discovered.
#
# set tapList [array names tagAttributes]
# set tapList [lsort $tapList]
# foreach tap $tapList {
#     set tap [string trim $tap \"]
#     #puts $tap
#     set tappair [split $tap ,]
#     set tag [lindex $tappair 0]
#     set attr [lindex $tappair 1]
# }

########################################################################
#
# JulianDate
#
# Compute the Julian date of a given date on the conventional calendar, given
# the month, day and year.
#
# If the 4th parameter is set, the Modified Julian Date is returned; otherwise
# the Julian Date is returned.
#
# Algorithm from pg 7 of Peter Duffet-Smith, 
# "Practical Astronomy With Your Calculator, 3d ed."
#
# Check against http://www.onlineconversion.com/julian_date.htm
# and <Orbiter distribution>/Utils/Date.exe
# This version does not check for Gregorian calendar or BC!!!
# This version does not handle fractional days!!!
#
proc JulianDate {Year Month Day {modified 0}} {
    if {($Month == 1)||($Month == 2)} {
        set Yprime [expr {$Year - 1}]
        set Mprime [expr {$Month + 12}]
    } else {
        set Yprime $Year
        set Mprime $Month
    }

    set A [expr {floor($Yprime / 100.0)}]
    set B [expr {2.0 - $A + floor($A / 4.0)}]

    set C [expr {floor(365.25 * $Yprime)}]

    set D [expr {floor(30.6001 * ($Mprime + 1.0))}]

    # Julian Day
    set JD  [expr {$B + $C + $D + $Day + 1720994.5}]
    
    # Modified Julian Day
    set MJD [expr {$B + $C + $D + $Day - 679006.0}]
    
    if {$modified} {
        return $MJD
    } else {
        return $JD
    }
}


########################################################################
#
# excelSerialDateToDateString
#
# See http://www.codeproject.com/KB/datetime/exceldmy.aspx
# and http://serendipity.magnet.ch/hermetic/cal_stud/jdn.htm
#
proc excelSerialDateToDateString {nSerialDate} {

    # TODO: fail for nSerialDate < 1

    # Modified Julian to DMY calculation with an addition of 2415019

    set l [expr {$nSerialDate + 68569.0 + 2415019.0}]
    set n [expr {int(( 4.0 * $l ) / 146097.0)}]
    set l [expr {$l - int(( 146097.0 * $n + 3.0 ) / 4.0)}]
    set i [expr {int(( 4000.0 * ( $l + 1.0 ) ) / 1461001.0)}]
    set l [expr {$l - int(( 1461.0 * $i ) / 4.0) + 31.0}]
    set j [expr {int(( 80.0 * $l ) / 2447.0)}]
    set nDay [expr {int($l - int(( 2447.0 * $j ) / 80.0))}]
    set l [expr {int($j / 11.0)}]
    set nMonth [expr {int($j + 2.0 - ( 12.0 * $l ))}]
    set nYear [expr {int(100.0 * ( $n - 49.0 ) + $i + $l)}]

    if {($nYear < 0)||($nYear > 9999)} {
        error "Illegal serial date $nSerialDate: couldn't parse to a legal date string."
    }

    return [format "%04u-%02u-%02u" $nYear $nMonth $nDay]
}

########################################################################
#
# dateStringToExcelSerialDate
#
proc dateStringToExcelSerialDate {dateString} {

    set nFields [scan $dateString "%04u-%02u-%02u" nYear nMonth nDay]

    if {$nFields != 3} {
        error 1 "Illegal date string $dateString"
    }

    if {($nYear < 0)||($nYear > 9999)|| \
            ($nMonth < 1)||($nMonth > 12)|| \
            ($nDay < 1)||($nDay > 31)} {
        error 1 "Illegal date string $dateString"
    }

    set MJD [JulianDate $nYear $nMonth $nDay 1]
    set nSerialDate [expr {int($MJD - 15018.0)}]

    return $nSerialDate
}

########################################################################
#
# calcBodyLeftMargin :private:
#
# given a list of body lines and a level, compute the number of spaces
# to shift the body lines to the right, i.e. the number of spaces to
# be removed from the left of each line. This is no more than (level+1)
# spaces, but if any of the lines have fewer than this many spaces, then
# the lesser number prevails.
#
proc calcBodyLeftMargin {body level} {
    if {$level == 0} {
        return 0
    } else {
        set nSpaces [expr {$level+1}]
    }

    foreach line $body {
        set lineNspaces \
            [expr {[string length $line] - \
                       [string length [string trimleft $line]]}]
        if {$lineNspaces < $nSpaces} {
            set nSpaces $lineNspaces
        }
    }

    return $nSpaces
}

########################################################################
#
# extractDatesFromBody
#
# given a list of body lines, find any lines for 
# returns {newBody dueDate doneDate} 
proc extractDatesFromBody {body} {
    # Find the CLOSED date, return it as the doneDate:
    # CLOSED:\s*\[([0-9]{4})-([0-9]{2})-([0-9]{2})

    set closedExp {CLOSED:\s*\[([0-9]{4})-([0-9]{2})-([0-9]{2})}
    set dueExp    {DEADLINE:\s*\<([0-9]{4})-([0-9]{2})-([0-9]{2})}

    set closedYYYY ""
    set closedMM ""
    set closedDD ""

    set dueYYYY ""
    set dueMM ""
    set dueDD ""

    set dueDate ""
    set doneDate ""

    set nLines [llength $body]
    set linesToRemove {} ; # a list of line indices to remove from body

    for {set i 0} {$i < $nLines} {incr i} {
        set nMatched [regexp -all $closedExp [lindex $body $i] \
                          allMatched closedYYYY closedMM closedDD]

        if {$nMatched > 0} {
            lappend linesToRemove $i

            set doneDate "$closedYYYY-$closedMM-$closedDD"
            break
        }
    }

    for {set i 0} {$i < $nLines} {incr i} {
        set nMatched [regexp -all $dueExp [lindex $body $i] \
                          allMatched dueYYYY dueMM dueDD]

        if {$nMatched > 0} {
            lappend linesToRemove $i

            set dueDate "$dueYYYY-$dueMM-$dueDD"
            break
        }
    }

    if {[llength $linesToRemove] > 0} {
        set linesToRemove [lsort -unique -decreasing -integer $linesToRemove]
        foreach i $linesToRemove {
            set body [lreplace $body $i $i]
        }
    }

    return [list $body $dueDate $doneDate]
}

########################################################################
#
# escXml 
#
# escape any characters which would confuse the XML interpreter.
#
proc escXml {str} {
    # Escape any embedded XML-special characters
    # The set is: " < > & '
    regsub -all {\&} $str {\&amp;} str
    regsub -all {\"} $str {\&quot;} str
    regsub -all {\'} $str {\&apos;} str
    regsub -all {\<} $str {\&lt;} str
    regsub -all {\>} $str {\&gt;} str
    return $str
}

########################################################################
#
# writeOrgIntoTDL :public:
#
proc writeOrgIntoTDL {orgTree} {
    set rv ""

    foreach {level headline tagsList propsList body children} $orgTree {}

    if {$level == 0} {
        # Level 0 corresponds to <TODOLIST>
        set rv "<?xml version=\"1.0\" ?>\n<TODOLIST FILEFORMAT=\"9\" PROJECTNAME=\"$headline\""

        if {$tagsList != {}} {
            puts stderr \
                "writeOrgIntoTDL: WARNING: non-null tags list, discarding..."
        }

        if {$propsList != {}} {
            puts stderr \
                "writeOrgIntoTDL: WARNING: non-null props list, discarding..."
        }

        if {$body != {}} {
            puts stderr \
                "writeOrgIntoTDL: WARNING: non-null body, discarding..."
        }

        if {$children == {}} {
            append rv "/>"
        } else {
            append rv ">\n"
            foreach child $children {
                append rv [writeOrgIntoTDL $child]
            }
            append rv "</TODOLIST>"
        }
    } else {

        set isDone 0
        # Org marks tasks with "TODO " and "DONE " to indicate their status.
        if {[string first "TODO " $headline] == 0} {
            set headline [string range $headline 5 end]
        } elseif {[string first "DONE " $headline] == 0} {
            set headline [string range $headline 5 end]
            set isDone 1
        }

        set rv "<TASK TITLE=\"[escXml $headline]\" "

        # Get the important dates buried in the body text.
        foreach {body dueDate doneDate} [extractDatesFromBody $body] {}

        # write out the tags as TDL categories
        set categoryNum 0
        foreach tag $tagsList {
            if {$categoryNum == 0} {
                set attName "CATEGORY"
            } else {
                set attName "CATEGORY$categoryNum"
            }
            append rv "${attName}=\"$tag\" "
            incr categoryNum
        }

        if {$dueDate != ""} {
            append rv "DUEDATE=\"[dateStringToExcelSerialDate $dueDate].0\" "
        }

        if {$isDone} {
            # We've marked something as done; did we remember when it was 
            # done?
            if {$doneDate == ""} {
                # Nope, set it to today.
                set dateString [clock format [clock seconds] -format "%Y-%m-%d"]
            } else {
                set dateString $doneDate
            }
            set doneDateSerial "[dateStringToExcelSerialDate $dateString].0"
            append rv "DONEDATE=\"$doneDateSerial\" "
        }

        # handle properties
        # TODO: What to do with properties which are not TDL_* properties?
        #puts "propsList=$propsList"
        foreach {key val} $propsList {
            #puts "key=$key val=$val"
            if {$key == "TDL_FILEREF"} {
                append rv "FILEREFPATH=\"[escXml $val]\" "
            } elseif {$key == "TDL_PERCENTDONE"} {
                append rv "PERCENTDONE=\"$val\" "
            } elseif {$key == "TDL_PRIORITY"} {
                append rv "PRIORITY=\"$val\" "
            } elseif {$key == "TDL_STATUS"} {
                append rv "STATUS=\"[escXml $val]\" "
            } elseif {$key == "TDL_PERSONS"} {
                set personsList [split $val ","]
                set personNum 0
                foreach person $personsList {
                    if {$personNum == 0} {
                        append rv "PERSON=\"[string trim [escXml $person]]\" "
                    } else {
                        append rv "PERSON${personNum}=\"[string trim [escXml $person]]\" "
                    }
                    incr personNum
                }
            }
        }

        if {$body != {}} {
            set newBody {}
            set leftMargin [calcBodyLeftMargin $body $level]
            foreach line $body {
                if {$leftMargin == 0} {
                    lappend newBody $line 
                } else {
                    lappend newBody [string range $line $leftMargin end]
                }
            }
            append rv "COMMENTS=\"[escXml [join $newBody \n]]\" "
        }

        if {[string index $rv end] == " "} {
            set rv [string trimright $rv]
        }

        if {$children == {}} {
            append rv "/>\n"
        } else {
            append rv ">\n"
            foreach child $children {
                append rv [writeOrgIntoTDL $child]
            }
            append rv "</TASK>"
        }
    }

    return $rv
}

########################################################################
########################################################################
########################################################################
########################################################################
########################################################################
########################################################################
##                                                                    ##
## TESTS                                                              ##
##                                                                    ##
########################################################################
########################################################################
########################################################################
########################################################################
########################################################################
########################################################################

########################################################################
# This is a baby unit test framework which is useful enough for the 
# continued development of this project.
########################################################################

########################################################################
#
# runTest
#
# run a test on some specific code and report back whether the returned value
# was the expected value.
#
# accepts a test as a list in the following format:
#
# { testName testCode expectedResult }
#
# runTest returns a 1 for a passed test and 0 for a failed test.
#
proc runTest {test {outChan stdout} {verbose 1}} {
    set name [lindex $test 0]
    set code [lindex $test 1]

    #puts "code = $code"

    set expectedResult [lindex $test 2]

    set result [eval $code]

    if {$result == $expectedResult} {
        if {$verbose} {
            puts $outChan "PASSED '$name'"
        }
        set rv 1
    } else {
        puts $outChan "FAILED '$name'"
        puts $outChan "       Expected:\[$expectedResult\]"
        puts $outChan "       Got     :\[$result\]"
        set rv 0
    }
    return $rv
}

########################################################################
# 
# runTests
#
# run a suite of tests and report back on the results.
#
proc runTests {tests include {verbose 1} {outChan stdout}} {
    set nTests [llength $tests]
    set testNum 0
    set nSuccessfulTests 0
    set failedTestNames {}

    # Empty include list means run them all
    if {[llength $include] != 0} {
        set nTests [llength $include]
    }
    
    foreach test $tests {
        incr testNum

        set testName [lindex $test 0]

        if {([llength $include] > 0) && ([lsearch $include $testName] == -1)} {
            continue
        }

        if {$verbose} {
            puts $outChan "------------------------------------------------------------------------"
            puts $outChan "Running ${testNum}/$nTests ... '$testName'"
        }
        set success  [runTest $test $outChan $verbose]
        if {$success} {
            incr nSuccessfulTests
        } else {
            lappend failedTestNames $testName
        }
        if {$verbose} {
            puts $outChan ""
        }
    }

    if {$verbose} {
        puts $outChan "------------------------------------------------------------------------"
    }
    puts $outChan "\nSuccessful tests = ${nSuccessfulTests}/$nTests"
    if {[llength $failedTestNames] > 0} {
        puts $outChan "\nFailed tests:\n"
        foreach failedName $failedTestNames {
            puts $outChan "* $failedName\n"
        }
    }
}

########################################################################
########################################################################
########################################################################

set tests {}

#------------------------------------------------------------------------
set name "Parse one headline, no body, 1 entry" 

set text {Here is my top-level headline.
* Level 1 :tag1:
Some text.}

set expectedResult {0 {Here is my top-level headline.} {} {} {} {{1 {Level 1} tag1 {} {{Some text.}} {}}}}

lappend tests [list $name [list parseTextIntoOrg $text] $expectedResult]

#-----------------------------------------------------------------------
set name "Parse plain text, no subheadings" 

set text {Here is a headline.
    And some more text.}

set expectedResult {0 {Here is a headline.} {} {} {{    And some more text.}} {}}

lappend tests [list $name [list parseTextIntoOrg $text] $expectedResult]

#-----------------------------------------------------------------------
set name "Parse headline + body, 1 entry"

set text {headline.
body.
* headline2
body 2.}

set expectedResult {0 headline. {} {} body. {{1 headline2 {} {} {{body 2.}} {}}}}

lappend tests [list $name [list parseTextIntoOrg $text ] $expectedResult]

#------------------------------------------------------------------------
set name "Expand one headline, no body, 1 entry" 

set expectedResult {Here is my top-level headline.
* Level 1                                                              :tag1:
Some text.}

set tree {0 {Here is my top-level headline.} {} {} {} {{1 {Level 1} tag1 {} {{Some text.}} {}}}}

lappend tests [list $name [list writeOrgIntoText $tree] $expectedResult]

#-----------------------------------------------------------------------
set name "Expand plain text, no subheadings" 

set expectedResult {Here is a headline.
    And some more text.}

set tree {0 {Here is a headline.} {} {} {{    And some more text.}} {}}

lappend tests [list $name [list writeOrgIntoText $tree] $expectedResult]

#-----------------------------------------------------------------------
set name "Expand headline + body, 1 entry"

set expectedResult {headline.
body.
* headline2
body 2.}

set tree {0 headline. {} {} body. {{1 headline2 {} {} {{body 2.}} {}}}}

lappend tests [list $name [list writeOrgIntoText $tree] $expectedResult]

#-----------------------------------------------------------------------
set name1 "Parse depth 3 tree with tags"
set name2 "Expand depth 3 tree with tags"

set text {Here is some Org-mode text.
It's not very exciting.

* NOTE Level 1 Note                                               :tag1:tag2:
  Has some more random musings in it.
** NOTE Level 2 Note
   Some more boring crap.
*** NOTE Level 3 Note                                                  :tag1:
*** NOTE another L3 Note
With some additional hoo-hah here.
}

set tree {0 {Here is some Org-mode text.} {} {} {{It's not very exciting.} {}} {{1 {NOTE Level 1 Note} {tag1 tag2} {} {{  Has some more random musings in it.}} {{2 {NOTE Level 2 Note} {} {} {{   Some more boring crap.}} {{3 {NOTE Level 3 Note} tag1 {} {} {}} {3 {NOTE another L3 Note} {} {} {{With some additional hoo-hah here.} {}} {}}}}}}}}

lappend tests [list $name1 [list parseTextIntoOrg $text] $tree]
lappend tests [list $name2 [list writeOrgIntoText $tree] $text]


#-----------------------------------------------------------------------
set name1 "Parse depth 2 tree without a document header"
set name2 "Expand depth 2 tree without a document header"

set text {* Level 1A
abcdef ghijk
** Level 2
lmnop qrstu
* Level 1B}

set tree {0 {} {} {} {} {{1 {Level 1A} {} {} {{abcdef ghijk}} {{2 {Level 2} {} {} {{lmnop qrstu}} {}}}} {1 {Level 1B} {} {} {} {}}}}

lappend tests [list $name1 [list parseTextIntoOrg $text] $tree]
lappend tests [list $name2 [list writeOrgIntoText $tree] $text]

#-----------------------------------------------------------------------
set name1 "Parse depth 1 tree without a document header"
set name2 "Expand depth 1 tree without a document header"

set text {* Level 1A}

set tree {0 {} {} {} {} {{1 {Level 1A} {} {} {} {}}}}

lappend tests [list $name1 [list parseTextIntoOrg $text 0] $tree]
lappend tests [list $name2 [list writeOrgIntoText $tree] $text]

#-----------------------------------------------------------------------
set name1 "Tricky: empty document header, level 2 is the first tree level"
set name2 "Tricky: expand empty document header, level 2 is the first tree level"

set text {** Level 2
How's this for tricky?}

set tree {0 {} {} {} {} {{1 {} {} {} {} {{2 {Level 2} {} {} {{How's this for tricky?}} {}}}}}}

lappend tests [list $name1 [list parseTextIntoOrg $text 0] $tree]
lappend tests [list $name2 [list writeOrgIntoText $tree] $text]

#-----------------------------------------------------------------------
set name1 "Tricky II: empty document header, level 2 is the first tree level"
set name2 "Tricky II: expand empty document header, level 2 is the first tree level"

set text {** Level 2
How's this for tricky?
** Level 2 again                                                        :tag:}

set tree {0 {} {} {} {} {{1 {} {} {} {} {{2 {Level 2} {} {} {{How's this for tricky?}} {}} {2 {Level 2 again} tag {} {} {}}}}}}

lappend tests [list $name1 [list parseTextIntoOrg $text 0] $tree]
lappend tests [list $name2 [list writeOrgIntoText $tree] $text]

#-----------------------------------------------------------------------
set name1 "Parse Org-mode splash text"
set name2 "Expand Org-mode splash text tree rep"

set text {* TODO Get new laptop                                                   :buy:

* PROJ Organize interstellar dust meeting                              :work:
*** DONE Book the meeting room
*** WAITING Organize LOC                                              :phone:
    Emails sent out, waiting for replies
*** STARTED Invited speakers [/]
    - [X] Draine
    - [ ] Tielens
    - [X] Hollenbach
*** TODO 1st Announcement                                            :URGENT:


* TODO Fix the bell in the hall                                        :home:}

set tree {0 {} {} {} {} {{1 {TODO Get new laptop} buy {} {{}} {}} {1 {PROJ Organize interstellar dust meeting} work {} {} {{2 {} {} {} {} {{3 {DONE Book the meeting room} {} {} {} {}} {3 {WAITING Organize LOC} phone {} {{    Emails sent out, waiting for replies}} {}} {3 {STARTED Invited speakers [/]} {} {} {{    - [X] Draine} {    - [ ] Tielens} {    - [X] Hollenbach}} {}} {3 {TODO 1st Announcement} URGENT {} {{} {}} {}}}}}} {1 {TODO Fix the bell in the hall} home {} {} {}}}}

lappend tests [list $name1 [list parseTextIntoOrg $text 0] $tree]
lappend tests [list $name2 [list writeOrgIntoText $tree] $text]

#-----------------------------------------------------------------------
set name1 "Parse multiline body"
set name2 "Expand including multiline body"

set text {Header
Here is a line

and here is another line
* First level
Two nonempty lines here
and so it goes
** Second level
Oh yeah
multiline, baby.}

set tree {0 Header {} {} {{Here is a line} {} {and here is another line}} {{1 {First level} {} {} {{Two nonempty lines here} {and so it goes}} {{2 {Second level} {} {} {{Oh yeah} {multiline, baby.}} {}}}}}}

lappend tests [list $name1 [list parseTextIntoOrg $text 0] $tree]
lappend tests [list $name2 [list writeOrgIntoText $tree] $text]

#-----------------------------------------------------------------------
set name1 "Parse workaday TODO list"
set name2 "Expand workaday TODO list"

set text {* TODO Continue work on parseOrg.tcl
  DEADLINE: <2010-12-24 Fri>
** Blank lines parsing and expansion not working
   Most everything else is working beautifully... should be ready for prime time
   soon.
}

set tree {0 {} {} {} {} {{1 {TODO Continue work on parseOrg.tcl} {} {} {{  DEADLINE: <2010-12-24 Fri>}} {{2 {Blank lines parsing and expansion not working} {} {} {{   Most everything else is working beautifully... should be ready for prime time} {   soon.} {}} {}}}}}}

lappend tests [list $name1 [list parseTextIntoOrg $text 0] $tree]
lappend tests [list $name2 [list writeOrgIntoText $tree] $text]

#-----------------------------------------------------------------------
set name2 "Expand final blank line"
set text {Hello there

How about this?}

set tree {0 {Hello there} {} {} {{} {How about this?}} {}}

lappend tests [list $name2 [list writeOrgIntoText $tree] $text]

#-----------------------------------------------------------------------
set name1 "Parse blank body lines"
set name2 "Expand including blank body lines"

set text {Header

* First level

** Second level

}

set tree {0 Header {} {} {{}} {{1 {First level} {} {} {{}} {{2 {Second level} {} {} {{} {}} {}}}}}}

lappend tests [list $name1 [list parseTextIntoOrg $text 0] $tree]
lappend tests [list $name2 [list writeOrgIntoText $tree] $text]

#------------------------------------------------------------------------
set name1 "Tricky III: Parse special characters"
set name2 "Tricky III: Parse special characters"

set text "OK tricks! {"
set tree {0 OK\ tricks!\ \{ {} {} {} {}}

lappend tests [list $name1 [list parseTextIntoOrg $text 0] $tree]
lappend tests [list $name2 [list writeOrgIntoText $tree] $text]

#-----------------------------------------------------------------------
set name1 "Tricky IV: Degenerate headers"
set text {*}

set tree {0 {} {} {} {} {}}

lappend tests [list $name1 [list parseTextIntoOrg $text 0] $tree]

#-----------------------------------------------------------------------
set name1 "Tricky V: Degenerate headers"
set text {* H1
*** H3

*******}

set tree {0 {} {} {} {} {{1 H1 {} {} {} {{2 {} {} {} {} {{3 H3 {} {} {{} *******} {}}}}}}}}

lappend tests [list $name1 [list parseTextIntoOrg $text 0] $tree]

#-----------------------------------------------------------------------
set name1 "Parse Empty headers"
set name2 "Parse Empty headers 2"
set name3 "Expand empty headers"

# NOTE This demonstrates an accidental feature of the parser. Empty headers will
# be silently discarded. Nodes which indicate level, but which have no titles or
# tags associated with them, will be detected, but not preserved upon output.

# This will parse into exactly the same tree as text2.
set text {* 
** A
** :A:B:
* 
** 
*** C
foo bar baz
}

# This will parse into exactly the same tree as text.
set text2 {** A
**                                                                      :A:B:
*** C
foo bar baz
}

# This will only expand into text2
set tree {0 {} {} {} {} {{1 {} {} {} {} {{2 A {} {} {} {}} {2 {} {A B} {} {} {}}}} {1 {} {} {} {} {{2 {} {} {} {} {{3 C {} {} {{foo bar baz} {}} {}}}}}}}}

lappend tests [list $name1 [list parseTextIntoOrg $text 0] $tree]
lappend tests [list $name2 [list writeOrgIntoText $tree] $text2]

#-----------------------------------------------------------------------
lappend tests [list "headline formatting 1" \
                   [list formatFirstLine 1 \
                        {TODO Get these apps for Android} \
                        android] \
                   {* TODO Get these apps for Android                                   :android:}]

lappend tests [list "headline formatting 2" \
                   [list formatFirstLine 1 \
                        {This is an unbelievably long header and will surely take much more space than necessary} \
                        {foo bar}] \
                   {* This is an unbelievably long header and will surely take much more space than necessary :foo:bar:} \
               ]

lappend tests [list "headline formatting 3" \
                   [list formatFirstLine 6 \
                        {The quick brown fox jumped etc.} \
                        {} ] \
                   {****** The quick brown fox jumped etc.}]

lappend tests [list "headline formatting 4" \
                   [list formatFirstLine 1 \
                        {TODO Get these apps for Android} \
                        android 0] \
                   {* TODO Get these apps for Android :android:}]

lappend tests [list "headline formatting 5" \
                   [list formatFirstLine 1 \
                        {This is an unbelievably long header and will surely take much more space than necessary} \
                        {foo bar} 0] \
                   {* This is an unbelievably long header and will surely take much more space than necessary :foo:bar:} \
               ]

lappend tests [list "headline formatting 6" \
                   [list formatFirstLine 6 \
                        {The quick brown fox jumped etc.} \
                        {} 0] \
                   {****** The quick brown fox jumped etc.}]

#-----------------------------------------------------------------------
set name1 "Reading properties"
set name2 "writing Properties"
set text {Header1
* TODO Task1
  :PROPERTIES:
  :foo: bar baz blue
  :rapunzel: let down your golden hair
  :END:

And some text.}

set tree {0 Header1 {} {} {} {{1 {TODO Task1} {} {foo {bar baz blue} rapunzel {let down your golden hair}} {{} {And some text.}} {}}}}

lappend tests [list $name1 [list parseTextIntoOrg $text] $tree]
lappend tests [list $name2 [list writeOrgIntoText $tree] $text]

########################################################################
#
# The stock tclparser doesn't handle this well: it strips out the LFs
# from tag attributes, replacing them all with spaces.
#
# See xml::tclparser::NormalizeAttValue for more details; in order
# to make this test work, I saw that this is the operative line:
#
#    regsub -all "\[$::xml::Wsp\]" $value { } value
#
# Which indicates that a good way to fix this is to redefine ::xml::Wsp
#

#-----------------------------------------------------------------------
set name1 "Parse XML containing LFs in the comments"

set xmlDoc {<?xml version="1.0" ?>
<TODOLIST>
        <TASK TITLE="First Task" COMMENTS="Line 1

Line 2
Line 3"/>
</TODOLIST>}

set orgDoc {* TODO First Task
  Line 1
  
  Line 2
  Line 3}

lappend tests [list $name1 [list parseTDLString $xmlDoc] $orgDoc]

#-----------------------------------------------------------------------
set name1 "Parse XML TDL: Categories into tags"

set xmlDoc {<?xml version="1.0" ?>
<TODOLIST>
    <TASK TITLE="First Task" COMMENTS="Line 1"/>
    <TASK TITLE="Second Task" CATEGORY="foo" CATEGORY1="bar"/>
</TODOLIST>}

set orgDoc {* TODO First Task
  Line 1
* TODO Second Task                                                  :foo:bar:}

lappend tests [list $name1 [list parseTDLString $xmlDoc] $orgDoc]

#-----------------------------------------------------------------------
set name1 "Parse XML TDL: Done date"

set xmlDoc {<?xml version="1.0" ?>
<TODOLIST>
    <TASK TITLE="First Task" COMMENTS="Line 1" DONEDATE="40540.0"/>
    <TASK TITLE="Second Task" CATEGORY="foo" CATEGORY1="bar"/>
</TODOLIST>}

set orgDoc {* DONE First Task
  CLOSED: [2010-12-28 Tue]
  Line 1
* TODO Second Task                                                  :foo:bar:}

lappend tests [list $name1 [list parseTDLString $xmlDoc] $orgDoc]

#-----------------------------------------------------------------------
set name1 "Parse XML TDL: Done date string"

set xmlDoc {<?xml version="1.0" ?>
<TODOLIST>
    <TASK TITLE="First Task" COMMENTS="Line 1" DONEDATESTRING="2010-12-28"/>
    <TASK TITLE="Second Task" CATEGORY="foo" CATEGORY1="bar"/>
</TODOLIST>}

set orgDoc {* DONE First Task
  CLOSED: [2010-12-28 Tue]
  Line 1
* TODO Second Task                                                  :foo:bar:}

lappend tests [list $name1 [list parseTDLString $xmlDoc] $orgDoc]

#-----------------------------------------------------------------------
set name1 "Parse XML TDL: Due date string"

set xmlDoc {<?xml version="1.0" ?>
<TODOLIST>
    <TASK TITLE="First Task" COMMENTS="Line 1" DUEDATESTRING="2010-12-28"/>
    <TASK TITLE="Second Task" CATEGORY="foo" CATEGORY1="bar"/>
</TODOLIST>}

set orgDoc {* TODO First Task
  DEADLINE: <2010-12-28 Tue>
  Line 1
* TODO Second Task                                                  :foo:bar:}

lappend tests [list $name1 [list parseTDLString $xmlDoc] $orgDoc]

#-----------------------------------------------------------------------
set name1 "Parse XML TDL: File ref string"

set xmlDoc {<?xml version="1.0" ?>
<TODOLIST>
    <TASK TITLE="First Task" COMMENTS="Line 1" DUEDATESTRING="2010-12-28"/>
    <TASK TITLE="Second Task" CATEGORY="foo" CATEGORY1="bar" FILEREFPATH="http://en.wikipedia.org/"/>
</TODOLIST>}

set orgDoc {* TODO First Task
  DEADLINE: <2010-12-28 Tue>
  Line 1
* TODO Second Task                                                  :foo:bar:
  :PROPERTIES:
  :TDL_FILEREF: http://en.wikipedia.org/
  :END:}

lappend tests [list $name1 [list parseTDLString $xmlDoc] $orgDoc]

#-----------------------------------------------------------------------
set name1 "Parse XML TDL: Percent done string"

set xmlDoc {<?xml version="1.0" ?>
<TODOLIST>
    <TASK TITLE="First Task" COMMENTS="Line 1" DUEDATESTRING="2010-12-28" PERCENTDONE="45">
      <TASK TITLE="Second Task" CATEGORY="foo" CATEGORY1="bar" FILEREFPATH="http://en.wikipedia.org/"/>
    </TASK>
</TODOLIST>}

set orgDoc {* TODO First Task
  DEADLINE: <2010-12-28 Tue>
  :PROPERTIES:
  :TDL_PERCENTDONE: 45
  :END:
  Line 1
** TODO Second Task                                                 :foo:bar:
   :PROPERTIES:
   :TDL_FILEREF: http://en.wikipedia.org/
   :END:}

lappend tests [list $name1 [list parseTDLString $xmlDoc] $orgDoc]

#-----------------------------------------------------------------------
set name1 "Parse XML TDL: Persons properties"

set xmlDoc {<?xml version="1.0" ?>
<TODOLIST>
    <TASK TITLE="First Task" PERSON="George" COMMENTS="Line 1" DUEDATESTRING="2010-12-28" PERCENTDONE="45" PERSON1="Martha" >
      <TASK TITLE="Second Task" CATEGORY="foo" CATEGORY1="bar" FILEREFPATH="http://en.wikipedia.org/" COMMENTS="Now here is a slightly more complicated example.
We have several persons attached to each task." PERSON="Alice" PERSON1="Bob"/>
    </TASK>
</TODOLIST>}

set orgDoc {* TODO First Task
  DEADLINE: <2010-12-28 Tue>
  :PROPERTIES:
  :TDL_PERSONS: George,Martha
  :TDL_PERCENTDONE: 45
  :END:
  Line 1
** TODO Second Task                                                 :foo:bar:
   :PROPERTIES:
   :TDL_PERSONS: Alice,Bob
   :TDL_FILEREF: http://en.wikipedia.org/
   :END:
   Now here is a slightly more complicated example.
   We have several persons attached to each task.}

lappend tests [list $name1 [list parseTDLString $xmlDoc] $orgDoc]

#-----------------------------------------------------------------------
set name1 "Parse XML TDL: Due date and completed date"

set xmlDoc {<?xml version="1.0" ?>
<TODOLIST>
    <TASK TITLE="First Task" COMMENTS="Line 1" DUEDATESTRING="2010-12-28" PERCENTDONE="45" DONEDATESTRING="2010-12-29">
      <TASK TITLE="Second Task" CATEGORY="foo" CATEGORY1="bar" FILEREFPATH="http://en.wikipedia.org/" COMMENTS="Now here is a slightly more complicated example.
We have several persons attached to each task." PERSON="Alice" PERSON1="Bob"/>
    </TASK>
</TODOLIST>}

set orgDoc {* DONE First Task
  DEADLINE: <2010-12-28 Tue> CLOSED: [2010-12-29 Wed]
  :PROPERTIES:
  :TDL_PERCENTDONE: 45
  :END:
  Line 1
** TODO Second Task                                                 :foo:bar:
   :PROPERTIES:
   :TDL_PERSONS: Alice,Bob
   :TDL_FILEREF: http://en.wikipedia.org/
   :END:
   Now here is a slightly more complicated example.
   We have several persons attached to each task.}

lappend tests [list $name1 [list parseTDLString $xmlDoc] $orgDoc]

#-----------------------------------------------------------------------
set name1 "Parse XML TDL: Priority and status"

set xmlDoc {<?xml version="1.0" ?>
<TODOLIST>
    <TASK TITLE="First Task" COMMENTS="Line 1" DUEDATESTRING="2010-12-28" PERCENTDONE="45" DONEDATESTRING="2010-12-29">
      <TASK TITLE="Second Task" CATEGORY="foo" CATEGORY1="bar" FILEREFPATH="http://en.wikipedia.org/" COMMENTS="Now here is a slightly more complicated example.
We have several persons attached to each task." PERSON="Alice" PERSON1="Bob" PRIORITY="7" STATUS="Not sure"/>
    </TASK>
</TODOLIST>}

set orgDoc {* DONE First Task
  DEADLINE: <2010-12-28 Tue> CLOSED: [2010-12-29 Wed]
  :PROPERTIES:
  :TDL_PERCENTDONE: 45
  :END:
  Line 1
** TODO Second Task                                                 :foo:bar:
   :PROPERTIES:
   :TDL_PERSONS: Alice,Bob
   :TDL_STATUS: Not sure
   :TDL_FILEREF: http://en.wikipedia.org/
   :TDL_PRIORITY: 7
   :END:
   Now here is a slightly more complicated example.
   We have several persons attached to each task.}

lappend tests [list $name1 [list parseTDLString $xmlDoc] $orgDoc]

#-----------------------------------------------------------------------
set name1 "Parse XML TDL into org"
set name2 "Parse org into XML TDL"

set xmlDoc {<?xml version="1.0" ?>
<TODOLIST FILEFORMAT="9" PROJECTNAME="Foo"/>}

set orgDoc {Foo}

lappend tests [list $name1 [list parseTDLString $xmlDoc] $orgDoc]
lappend tests [list $name2 [list writeOrgIntoTDL [parseTextIntoOrg $orgDoc]] $xmlDoc]

#-----------------------------------------------------------------------
set name "calcBodyLeftMargin"

lappend tests [list "calcBodyLeftMargin 1" [list calcBodyLeftMargin {   {    Foo    }
                                 {    grate}
                                 { Ban     }
                                 {    Bore    }} 2] 1 ]

lappend tests [list "calcBodyLeftMargin 2" [list calcBodyLeftMargin {   {    Foo    }
                                 {    grate}
                                 {    Ban     }
                                 {    Bore    }} 2] 3 ]

lappend tests [list "calcBodyLeftMargin 3" [list calcBodyLeftMargin {   {    Foo    }
                                 {       grate}
                                 {    Ban     }
                                 {    Bore    }} 3] 4 ]


#-----------------------------------------------------------------------
# Date tests
#

# Excel serial date, calendar date, Julian date (at 0:00:00), MJD, Day of Week
set dateTests {
    {0     "1899-12-30"    2415018.5     15018.0    Sat}
    {1     "1899-12-31"    2415019.5     15019.0    Sun}
    {2     "1900-01-01"    2415020.5     15020.0    Mon} 
    {60    "1900-02-28"    2415078.5     15078.0    Wed}
    {61    "1900-03-01"    2415079.5     15079.0    Thu}
    {37477 "2002-08-09"    2452495.5     52495.0    Fri}
    {39688 "2008-08-28"    2454706.5     54706.0    Thu}
    {40353 "2010-06-24"    2455371.5     55371.0    Thu}
    {40544 "2011-01-01"    2455562.5     55562.0    Sat}
}

set testNumber 1
foreach test $dateTests {
    lappend tests [list "Serial date to string $testNumber" \
                       [list excelSerialDateToDateString \
                            [lindex $test 0]] [lindex $test 1]]
    incr testNumber
}

set testNumber 1
foreach test $dateTests {
    lappend tests [list "String to Serial date $testNumber" \
                       [list dateStringToExcelSerialDate \
                            [lindex $test 1]] [lindex $test 0]]
    incr testNumber
}

#-----------------------------------------------------------------------
set name1 "Parse XML TDL into org 2"
set name2 "Parse org into XML TDL 2"

set xmlDoc {<?xml version="1.0" ?>
<TODOLIST FILEFORMAT="9" PROJECTNAME="Foo">
<TASK TITLE="First Task" COMMENTS="Plans that either come to naught
Or half a page of scribbled lines"/>
</TODOLIST>}

set orgDoc {Foo
* TODO First Task
  Plans that either come to naught
  Or half a page of scribbled lines}

lappend tests [list $name1 [list parseTDLString $xmlDoc] $orgDoc]
lappend tests [list $name2 [list writeOrgIntoTDL [parseTextIntoOrg $orgDoc]] $xmlDoc]

#-----------------------------------------------------------------------
set name1 "Parse XML TDL into org 3"
set name2 "Parse org into XML TDL 3"

set xmlDoc {<?xml version="1.0" ?>
<TODOLIST FILEFORMAT="9" PROJECTNAME="Foo">
<TASK TITLE="First Task" COMMENTS="Plans that either come to naught
Or half a page of scribbled lines"/>
<TASK TITLE="Second Task" DONEDATE="40544.0" PERCENTDONE="100" COMMENTS="This one should be marked complete."/>
</TODOLIST>}

set orgDoc {Foo
* TODO First Task
  Plans that either come to naught
  Or half a page of scribbled lines
* DONE Second Task
  CLOSED: [2011-01-01 Sat]
  :PROPERTIES:
  :TDL_PERCENTDONE: 100
  :END:
  This one should be marked complete.}

lappend tests [list $name1 [list parseTDLString $xmlDoc] $orgDoc]
lappend tests [list $name2 [list writeOrgIntoTDL [parseTextIntoOrg $orgDoc]] $xmlDoc]

#-----------------------------------------------------------------------
set name1 "Parse XML TDL into org 4"
set name2 "Parse org into XML TDL 4"

set xmlDoc {<?xml version="1.0" ?>
<TODOLIST FILEFORMAT="9" PROJECTNAME="Test2">
<TASK TITLE="Second Task" DUEDATE="40545.0" DONEDATE="40544.0" PERCENTDONE="100" COMMENTS="This one should be marked complete."/>
</TODOLIST>}

set orgDoc {Test2
* DONE Second Task
  DEADLINE: <2011-01-02 Sun> CLOSED: [2011-01-01 Sat]
  :PROPERTIES:
  :TDL_PERCENTDONE: 100
  :END:
  This one should be marked complete.}

lappend tests [list $name1 [list parseTDLString $xmlDoc] $orgDoc]
lappend tests [list $name2 [list writeOrgIntoTDL [parseTextIntoOrg $orgDoc]] $xmlDoc]

#-----------------------------------------------------------------------
set name1 "Parse XML TDL into org 5"
set name2 "Parse org into XML TDL 5"

set xmlDoc {<?xml version="1.0" ?>
<TODOLIST FILEFORMAT="9" PROJECTNAME="Test2">
<TASK TITLE="Second Task" DUEDATE="40545.0" PERCENTDONE="50" STATUS="Working on it" COMMENTS="This one should NOT be marked complete."/>
</TODOLIST>}

set orgDoc {Test2
* TODO Second Task
  DEADLINE: <2011-01-02 Sun>
  :PROPERTIES:
  :TDL_PERCENTDONE: 50
  :TDL_STATUS: Working on it
  :END:
  This one should NOT be marked complete.}

lappend tests [list $name1 [list parseTDLString $xmlDoc] $orgDoc]
lappend tests [list $name2 [list writeOrgIntoTDL [parseTextIntoOrg $orgDoc]] $xmlDoc]

#-----------------------------------------------------------------------
set name1 "Parse XML TDL into org 6"
set name2 "Parse org into XML TDL 6"

set xmlDoc {<?xml version="1.0" ?>
<TODOLIST FILEFORMAT="9" PROJECTNAME="Test2">
<TASK TITLE="Second Task" DUEDATE="40545.0" PERSON="Alice Foo" PERSON1="Bob Stone" PERSON2="Carol" PERSON3="Dave" PERCENTDONE="50" STATUS="Working on it" COMMENTS="This one should NOT be marked complete."/>
</TODOLIST>}

set orgDoc {Test2
* TODO Second Task
  DEADLINE: <2011-01-02 Sun>
  :PROPERTIES:
  :TDL_PERSONS: Alice Foo,Bob Stone,Carol,Dave
  :TDL_PERCENTDONE: 50
  :TDL_STATUS: Working on it
  :END:
  This one should NOT be marked complete.}

lappend tests [list $name1 [list parseTDLString $xmlDoc] $orgDoc]
lappend tests [list $name2 [list writeOrgIntoTDL [parseTextIntoOrg $orgDoc]] $xmlDoc]

#-----------------------------------------------------------------------
set name1 "Parse XML TDL into org 7"
set name2 "Parse org into XML TDL 7"

set xmlDoc {<?xml version="1.0" ?>
<TODOLIST FILEFORMAT="9" PROJECTNAME="Test2">
<TASK TITLE="Second Task" CATEGORY="work" CATEGORY1="life" DUEDATE="40545.0" PERSON="Alice Foo" PERSON1="Bob Stone" PERSON2="Carol" PERSON3="Dave" PERCENTDONE="50" STATUS="Working on it" COMMENTS="This one should NOT be marked complete."/>
</TODOLIST>}

set orgDoc {Test2
* TODO Second Task                                                :work:life:
  DEADLINE: <2011-01-02 Sun>
  :PROPERTIES:
  :TDL_PERSONS: Alice Foo,Bob Stone,Carol,Dave
  :TDL_PERCENTDONE: 50
  :TDL_STATUS: Working on it
  :END:
  This one should NOT be marked complete.}

lappend tests [list $name1 [list parseTDLString $xmlDoc] $orgDoc]
lappend tests [list $name2 [list writeOrgIntoTDL [parseTextIntoOrg $orgDoc]] $xmlDoc]


#-----------------------------------------------------------------------
set name1 "Parse XML TDL into org 8"
set name2 "Parse org into XML TDL 8"

set xmlDoc {<?xml version="1.0" ?>
<TODOLIST FILEFORMAT="9" PROJECTNAME="Test2">
<TASK TITLE="Second Task" CATEGORY="work" CATEGORY1="life" DUEDATE="40545.0" PERSON="Alice Foo" PERSON1="Bob Stone" PERSON2="Carol" PERSON3="Dave" PERCENTDONE="50" STATUS="Working on it" FILEREFPATH="http://www.google.com/" PRIORITY="0" COMMENTS="This one should NOT be marked complete."/>
</TODOLIST>}

set orgDoc {Test2
* TODO Second Task                                                :work:life:
  DEADLINE: <2011-01-02 Sun>
  :PROPERTIES:
  :TDL_PERSONS: Alice Foo,Bob Stone,Carol,Dave
  :TDL_PERCENTDONE: 50
  :TDL_STATUS: Working on it
  :TDL_FILEREF: http://www.google.com/
  :TDL_PRIORITY: 0
  :END:
  This one should NOT be marked complete.}

lappend tests [list $name1 [list parseTDLString $xmlDoc] $orgDoc]
lappend tests [list $name2 [list writeOrgIntoTDL [parseTextIntoOrg $orgDoc]] $xmlDoc]
#-----------------------------------------------------------------------
set name1 "Parse XML TDL into org 9"
set name2 "Parse org into XML TDL 9"

set xmlDoc {<?xml version="1.0" ?>
<TODOLIST FILEFORMAT="9" PROJECTNAME="Test2">
<TASK TITLE="Second Task" CATEGORY="work" CATEGORY1="life" DONEDATE="40545.0" PERSON="Alice Foo" PERSON1="Bob Stone" PERSON2="Carol" PERSON3="Dave" PERCENTDONE="50" STATUS="Working on it" FILEREFPATH="http://www.google.com/" PRIORITY="0" COMMENTS="This one should be marked complete."/>
</TODOLIST>}

set orgDoc {Test2
* DONE Second Task                                                :work:life:
  CLOSED: [2011-01-02 Sun]
  :PROPERTIES:
  :TDL_PERSONS: Alice Foo,Bob Stone,Carol,Dave
  :TDL_PERCENTDONE: 50
  :TDL_STATUS: Working on it
  :TDL_FILEREF: http://www.google.com/
  :TDL_PRIORITY: 0
  :END:
  This one should be marked complete.}

lappend tests [list $name1 [list parseTDLString $xmlDoc] $orgDoc]
lappend tests [list $name2 [list writeOrgIntoTDL [parseTextIntoOrg $orgDoc]] $xmlDoc]

#-----------------------------------------------------------------------
set name1 "Parse XML TDL with embedded quotes"
set name2 "Parse Org into TDL with embedded quotes"

set xmlDoc {<?xml version="1.0" ?>
<TODOLIST FILEFORMAT="9" PROJECTNAME="">
<TASK TITLE="Embedded quotes!" COMMENTS="This one should have some &quot;kick&quot; to it!"/>
</TODOLIST>}

set orgDoc {* TODO Embedded quotes!
  This one should have some "kick" to it!}

lappend tests [list $name1 [list parseTDLString $xmlDoc] $orgDoc]
lappend tests [list $name2 [list writeOrgIntoTDL [parseTextIntoOrg $orgDoc]] $xmlDoc]

#-----------------------------------------------------------------------
set name1 "Parse XML TDL with embedded angle brackets"
set name2 "Parse Org into TDL with angle brackets"

set xmlDoc {<?xml version="1.0" ?>
<TODOLIST FILEFORMAT="9" PROJECTNAME="">
<TASK TITLE="Embedded angle brackets!" COMMENTS="This one should have some &lt;kick&gt; to it!"/>
</TODOLIST>}

set orgDoc {* TODO Embedded angle brackets!
  This one should have some <kick> to it!}

lappend tests [list $name1 [list parseTDLString $xmlDoc] $orgDoc]
lappend tests [list $name2 [list writeOrgIntoTDL [parseTextIntoOrg $orgDoc]] $xmlDoc]

#-----------------------------------------------------------------------
set name1 "Parse XML TDL with embedded special characters"
set name2 "Parse Org into TDL embedded special characters"

set xmlDoc {<?xml version="1.0" ?>
<TODOLIST FILEFORMAT="9" PROJECTNAME="">
<TASK TITLE="Embedded special characters!" COMMENTS="&amp; &lt; &gt; &quot;"/>
</TODOLIST>}

set orgDoc "* TODO Embedded special characters!\n  \& \< \> \""

lappend tests [list $name1 [list parseTDLString $xmlDoc] $orgDoc]
lappend tests [list $name2 [list writeOrgIntoTDL [parseTextIntoOrg $orgDoc]] $xmlDoc]

#-----------------------------------------------------------------------
set name1 "parseArgs: empty list"
set args {}
set struct {1 {No action selected.} 0 0 0 0 0 0 -1 -1 {} {} 0}

lappend tests [list $name1 [list parseArgs $args] $struct]

#-----------------------------------------------------------------------
set name1 "parseArgs: org2tdl"
set args {-org2tdl}
set struct {0 {No error} 1 0 0 0 0 0 -1 -1 {} {} 0}

lappend tests [list $name1 [list parseArgs $args] $struct]

#-----------------------------------------------------------------------
set name1 "parseArgs: tdl2org"
set args {-tdl2org}
set struct {0 {No error} 0 1 0 0 0 0 -1 -1 {} {} 0}

lappend tests [list $name1 [list parseArgs $args] $struct]

#-----------------------------------------------------------------------
set name1 "parseArgs: test"
set args {-test}
set struct {0 {No error} 0 0 1 0 0 0 -1 -1 {} {} 0}

lappend tests [list $name1 [list parseArgs $args] $struct]

#-----------------------------------------------------------------------
set name1 "parseArgs: done"
set args {-done}
set struct {0 {No error} 0 0 0 1 1 1 -1 -1 {} {} 0}

lappend tests [list $name1 [list parseArgs $args] $struct]

#-----------------------------------------------------------------------
set name1 "parseArgs: unknown option"
set args {-foo}
set struct {1 {Unknown option -foo} 0 0 0 0 0 0 -1 -1 {} {} 0}

lappend tests [list $name1 [list parseArgs $args] $struct]

#-----------------------------------------------------------------------
set name1 "parseArgs: Multiple options"
set args {-test -tdl2org}
set struct {1 {Multiple modes selected, aborting.} 0 1 1 0 0 0 -1 -1 {} {} 0}

lappend tests [list $name1 [list parseArgs $args] $struct]

#-----------------------------------------------------------------------
set name1 "parseArgs: input file"
set args {-tdl2org -i foo.tdl}
set struct {0 {No error} 0 1 0 0 0 0 -1 -1 foo.tdl {} 0}

lappend tests [list $name1 [list parseArgs $args] $struct]

#-----------------------------------------------------------------------
set name1 "parseArgs: no input file"
set args {-tdl2org -i}
set struct {1 {No input filename} 0 1 0 0 0 0 -1 -1 {} {} 0}

lappend tests [list $name1 [list parseArgs $args] $struct]

#-----------------------------------------------------------------------
set name1 "parseArgs: output file"
set args {-org2tdl -o foo.tdl}
set struct {0 {No error} 1 0 0 0 0 0 -1 -1 {} foo.tdl 0}

lappend tests [list $name1 [list parseArgs $args] $struct]

#-----------------------------------------------------------------------
set name1 "parseArgs: no input file"
set args {-org2tdl -o}
set struct {1 {No output filename} 1 0 0 0 0 0 -1 -1 {} {} 0}

lappend tests [list $name1 [list parseArgs $args] $struct]

#-----------------------------------------------------------------------
set name1 "parseArgs: input and output files specified"
set args {-test -i foo.txt -o bar.txt}
set struct {0 {No error} 0 0 1 0 0 0 -1 -1 foo.txt bar.txt 0}

lappend tests [list $name1 [list parseArgs $args] $struct]

#-----------------------------------------------------------------------
set name1 "parseArgs: -done with -to < -from"
set args {-done -from 2008-01-12 -to 2005-02-16}
set struct {1 {Start Date > End Date} 0 0 0 1 1 0 39459 38399 {} {} 0}

lappend tests [list $name1 [list parseArgs $args] $struct]

#-----------------------------------------------------------------------
set name1 "parseArgs: -done with -to and -from"
set args {-done -from 2001-01-12 -to 2005-02-16}
set struct {0 {No error} 0 0 0 1 1 0 36903 38399 {} {} 0}

lappend tests [list $name1 [list parseArgs $args] $struct]

#-----------------------------------------------------------------------
set name1 "parseArgs: -from with no date (last argument) (should fail)"
set args {-done -from }
set struct {1 {No from date} 0 0 0 1 1 1 -1 -1 {} {} 0}

lappend tests [list $name1 [list parseArgs $args] $struct]

#-----------------------------------------------------------------------
set name1 "parseArgs: -from with to date (last argument) (should fail)"
set args {-done -to }
set struct {1 {No to date} 0 0 0 1 1 1 -1 -1 {} {} 0}

lappend tests [list $name1 [list parseArgs $args] $struct]

#-----------------------------------------------------------------------
set name1 "parseArgs: -done with -from crazy date"
set args {-done -from 1870-01-03}
set struct {1 {Badly formed from date} 0 0 0 1 1 1 -1 -1 {} {} 0}

lappend tests [list $name1 [list parseArgs $args] $struct]

#-----------------------------------------------------------------------
set name1 "parseArgs: -done with -from Badly formed from date"
set args {-done -from XyZABC}
set struct {1 {Badly formed from date} 0 0 0 1 1 1 -1 -1 {} {} 0}

lappend tests [list $name1 [list parseArgs $args] $struct]

#-----------------------------------------------------------------------
set name1 "parseArgs: -done with -from Badly formed to date"
set args {-done -from 2011-01-12 -to FooGrabMe}
set struct {1 {Badly formed to date} 0 0 0 1 1 0 40555 -1 {} {} 0}

lappend tests [list $name1 [list parseArgs $args] $struct]

#-----------------------------------------------------------------------
set name1 "parseArgs: -done with -from Badly formed to date 2"
set args {-done -from 2011-01-12 -to 2012}
set struct {1 {Badly formed to date} 0 0 0 1 1 0 40555 -1 {} {} 0}

lappend tests [list $name1 [list parseArgs $args] $struct]

#-----------------------------------------------------------------------
set name1 "parseArgs: -from without -done (should fail)"
set args {-from 2011-01-12}
set struct {1 {-from specified without preceding -done} 0 0 0 0 0 0 -1 -1 {} {} 0}

lappend tests [list $name1 [list parseArgs $args] $struct]

#-----------------------------------------------------------------------
set name1 "parseArgs: -to without -done (should fail)"
set args {-to 2011-01-12}
set struct {1 {-to specified without preceding -done} 0 0 0 0 0 0 -1 -1 {} {} 0}

lappend tests [list $name1 [list parseArgs $args] $struct]

#-----------------------------------------------------------------------
set name1 "parseArgs: -datespan with no -done (should fail)"
set args {-datespan 5}
set struct {1 {-datespan specified without preceding -done} 0 0 0 0 0 0 -1 -1 {} {} 0}

lappend tests [list $name1 [list parseArgs $args] $struct]

#-----------------------------------------------------------------------
set name1 "parseArgs: -datespan with no -from (should fail)"
set args {-done -datespan 5}
set struct {1 {-datespan specified without preceding -from date} 0 0 0 1 1 1 -1 -1 {} {} 0}

lappend tests [list $name1 [list parseArgs $args] $struct]

#-----------------------------------------------------------------------
set name1 "parseArgs: -datespan without argument (should fail)"
set args {-done -from 2010-07-13 -datespan}
set struct {1 {No datespan specified} 0 0 0 1 1 0 40372 -1 {} {} 0}

lappend tests [list $name1 [list parseArgs $args] $struct]

#-----------------------------------------------------------------------
set name1 "parseArgs: -datespan with badly-formed number (should fail)"
set args {-done -from 2010-07-13 -datespan three}
set struct {1 {Badly formed span value} 0 0 0 1 1 0 40372 -1 {} {} 0}

lappend tests [list $name1 [list parseArgs $args] $struct]

#-----------------------------------------------------------------------
set name1 "parseArgs: -datespan good"
set args {-done -from 2010-07-13 -datespan 5}
set struct {0 {No error} 0 0 0 1 1 0 40372 40377 {} {} 0}

lappend tests [list $name1 [list parseArgs $args] $struct]

#-----------------------------------------------------------------------
set name1 "parseArgs: -from -to and -undated (should fail)"
set args {-done -from 2010-07-13 -datespan 5 -undated}
set struct {1 {Cannot select both dated and undated items} 0 0 0 1 1 1 40372 40377 {} {} 0}

lappend tests [list $name1 [list parseArgs $args] $struct]

#-----------------------------------------------------------------------
set name1 "parseArgs: -undated without -done"
set args {-undated}
set struct {1 {-undated specified without preceding -done} 0 0 0 0 0 0 -1 -1 {} {} 0}

lappend tests [list $name1 [list parseArgs $args] $struct]

#-----------------------------------------------------------------------
set name1 "parseArgs: -undated all correct"
set args {-done -undated}
set struct {0 {No error} 0 0 0 1 0 1 -1 -1 {} {} 0}

lappend tests [list $name1 [list parseArgs $args] $struct]

#-----------------------------------------------------------------------
set name1 "parseArgs: -to with -done but no -from (OK, should succeed)"
set args {-done -to 2010-07-04}
set struct {0 {No error} 0 0 0 1 1 0 -1 40363 {} {} 0}

lappend tests [list $name1 [list parseArgs $args] $struct]

#-----------------------------------------------------------------------
set name1 "parseArgs: -from with -done but no -to (OK, should succeed)"
set args {-done -from 2010-07-04}
set struct {0 {No error} 0 0 0 1 1 0 40363 -1 {} {} 0}

lappend tests [list $name1 [list parseArgs $args] $struct]

#-----------------------------------------------------------------------
set name1 "parseArgs: -n without -done (should fail)"
set args {-n}
set struct {1 {-n specified without preceding -done} 0 0 0 0 0 0 -1 -1 {} {} 0}

lappend tests [list $name1 [list parseArgs $args] $struct]

#-----------------------------------------------------------------------
set name1 "parseArgs: -n"
set args {-done -n}
set struct {0 {No error} 0 0 0 1 1 1 -1 -1 {} {} 1}

lappend tests [list $name1 [list parseArgs $args] $struct]

#-----------------------------------------------------------------------
set name1 "parseArgs: -dated"
set args {-done -dated}
set struct {0 {No error} 0 0 0 1 1 0 -1 -1 {} {} 0}

lappend tests [list $name1 [list parseArgs $args] $struct]

#-----------------------------------------------------------------------
set name1 "parseArgs: -dated without -done"
set args {-dated}
set struct {1 {-dated specified without preceding -done} 0 0 0 0 0 0 -1 -1 {} {} 0}

lappend tests [list $name1 [list parseArgs $args] $struct]





# TODO more tests
# Test numbered done tasks
# Test date functionality
# parseArgs: -datespan with no -from (should fail)

#-----------------------------------------------------------------------
set name1 "doneitems 1"
set orgDoc {* TODO First Task
** DONE Second Task
* DONE Third Task          :foo:bar:
  You know, there's some text here.
** TODO Fourth Task
}
set doneList {{DONE Second Task} {DONE Third Task}}

lappend tests [list $name1 [list getDoneHeadlines $orgDoc] $doneList]

#-----------------------------------------------------------------------
set name1 "doneitems 2"
set orgDoc {* TODO First Task
** DONE Second Task
* DONE Third Task          :foo:bar:
  You know, there's some text here.
** TODO Fourth Task
*** DONE Fifth Task
**** TODO and here's another
**** DONE and another.
* DONE last task.
}
set doneList {{DONE Second Task} {DONE Third Task} {DONE Fifth Task} {DONE and another.} {DONE last task.}}

lappend tests [list $name1 [list getDoneHeadlines $orgDoc] $doneList]

#-----------------------------------------------------------------------

lappend tests [list {eligibleForDoneList test: dated and undated} \
                   [list eligibleForDoneList "" 1 1 -1 -1] 1]
lappend tests [list {eligibleForDoneList test: undated 1} \
                   [list eligibleForDoneList "" 0 1 -1 -1] 1]
lappend tests [list {eligibleForDoneList test: undated 2} \
                   [list eligibleForDoneList "2011-04-21" 0 1 -1 -1] 0]
lappend tests [list {eligibleForDoneList test: dated 1} \
                   [list eligibleForDoneList "2011-04-21" 1 0 -1 -1] 1]
lappend tests [list {eligibleForDoneList test: dated 2} \
                   [list eligibleForDoneList "2011-04-21" 1 0 37000 -1] 1]
lappend tests [list {eligibleForDoneList test: dated 3} \
                   [list eligibleForDoneList "2011-04-21" 1 0 -1 41000] 1]
lappend tests [list {eligibleForDoneList test: dated 4} \
                   [list eligibleForDoneList "2011-04-21" 1 0 37000 41000] 1]
lappend tests [list {eligibleForDoneList test: dated 5} \
                   [list eligibleForDoneList "2011-04-21" 1 0 40700 41000] 0]
lappend tests [list {eligibleForDoneList test: dated 6} \
                   [list eligibleForDoneList "2011-04-21" 1 0 40700 -1] 0]
lappend tests [list {eligibleForDoneList test: dated 7} \
                   [list eligibleForDoneList "2011-04-21" 1 0 37000 40500 ] 0]


########################################################################
########################################################################
########################################################################
########################################################################
########################################################################
########################################################################
#                                                                      #
# Public entry points                                                  #
#                                                                      #
########################################################################
########################################################################
########################################################################
########################################################################
########################################################################
########################################################################

########################################################################
# 
# TDL2Org :public:
#
# TDL2Org <tdlfile> <orgfile>
#
# returns 0 if no error, -1 otherwise
#
proc TDL2Org {tdlFile orgFile} {
    if {($orgFile != "") && [file exists $orgFile]} {
        puts stderr "TDL2Org: output file $orgFile already exists, aborting."
        return -1
    }

    if {($tdlFile != "") && ![file exists $tdlFile]} {
        puts stderr "TDL2Org: input file $tdlFile does not exist, aborting."
        return -1
    }

    if {$orgFile == ""} {
        set outchan stdout
    } else {
        set outchan [open $orgFile "w"]
    }

    puts $outchan [parseTDLFile $tdlFile]

    if {$orgFile != ""} {
        close $outchan
    }

    return 0
}

########################################################################
#
# Org2TDL :public:
# 
# Org2TDL <orgfile> <tdlfile>
#
# returns 0 if no error, -1 otherwise
#
proc Org2TDL {orgFile tdlFile} {
    if {($tdlFile != "") && [file exists $tdlFile]} {
        puts stderr "Org2TDL: output file $tdlFile already exists, aborting."
        return -1
    }

    if {($orgFile != "") && ![file exists $orgFile]} {
        puts stderr "Org2TDL: input file $orgFile does not exist, aborting."
        return -1
    }

    if {$tdlFile != ""} {
        set outchan [open $tdlFile "w"]
    } else {
        set outchan stdout
    }
    
    puts $outchan [writeOrgIntoTDL [parseOrgFile $orgFile]]
    
    if {$tdlFile != ""} {
        close $outchan
    }

    return 0
}

########################################################################
#
# DoneForFile :public:
# 
# DoneForFile <orgfile> <outfile>
#
# returns 0 if no error, -1 otherwise
#
proc DoneForFile {orgFile outFile dated undated startDate endDate lineNumbers} {
    if {($outFile != "") && [file exists $outFile]} {
        puts stderr "DoneForFile: output file $outFile already exists, aborting."
        return -1
    }

    if {($orgFile != "") && ![file exists $orgFile]} {
        puts stderr "DoneForFile: input file $orgFile does not exist, aborting."
        return -1
    }

    if {$outFile != ""} {
        set outchan [open $outFile "w"]
    } else {
        set outchan stdout
    }
    
    puts $outchan [lineBufferToText [getDoneHeadlinesFromOrgList \
                      [parseOrgFile $orgFile] {} \
                          $dated $undated $startDate $endDate $lineNumbers]]
    
    if {$outFile != ""} {
        close $outchan
    }

    return 0
}


########################################################################
# 
# Usage
#
proc putUsage {{chan stderr}} {
    puts $chan "USAGE:"
    puts $chan "orgutil \[-org2tdl|-tdl2org|-test|-done\]"
    puts $chan "        \[-from YYYY-MM-DD\] \[-to YYYY-MM-DD | -datespan n\]"
    puts $chan "        \[-undated\] \[-dated\] \[-n\]"
    puts $chan "        \[-i <file>\] \[-o <file>\]"
    puts $chan " "
    puts $chan "-org2tdl         : convert Org-mode file to TDL"
    puts $chan "-tdl2org         : convert TDL file to Org-mode"
    puts $chan "-done            : scan input file for DONE items and report"
    puts $chan "-from YYYY-MM-DD : (DONE only) report items done >= date"
    puts $chan "-to YYYY-MM-DD   : (DONE only) report items done <= date"
    puts $chan "-datespan n      : (DONE only) like -to but use a span of days"
    puts $chan "-n               : (DONE only) add line numbers to date report"
    puts $chan "-undated         : (DONE only) return undated done items"
    puts $chan "-test            : run tests (file args ignored)"
    puts $chan "-i <file>        : specify input file"
    puts $chan "-o <file>        : specify output file"
    puts $chan " " 
    puts $chan "If input filename is not supplied, stdin will be used."
    puts $chan "If output filename is not supplied, stdout will be used."
}

########################################################################
#
# parseArgs
#
# break down the arguments to the program and send back a structure
# which indicates the work to be performed.
#
# orgutil [-org2tdl|-tdl2org|-test|-done] [-i <file>] [-o <file>]
#
# 
proc parseArgs {argv} {
    set errorcode 0;                # Parsing error code
    set errormessage "No error";    # Corresponding message
    set org2tdlmode 0;              # Convert org-mode to tdl
    set tdl2orgmode 0;              # Convert tdl to org-mode
    set donemode 0;                 # Report on DONE items
    set testmode 0;                 # Run tests
    set dated 0;                    # For DONE report: dated items
    set undated 0;                  # For DONE report: undated items
    set startDate -1;               # For DONE report: start date
    set endDate -1;                 # For DONE report: end date
    set lineNumbers 0;              # For DONE report: line numbers
    set infilename ""
    set outfilename ""

    set argc [llength $argv]
    for {set i 0} {$i < $argc} {incr i} {

        set arg [lindex $argv $i]

        if {$arg == "-org2tdl"} {
            set org2tdlmode 1
        } elseif {$arg == "-tdl2org"} {
            set tdl2orgmode 1
        } elseif {$arg == "-test"} {
            set testmode 1
        } elseif {$arg == "-done"} {
            set donemode 1
        } elseif {$arg == "-from"} {
            if {$donemode == 0} {
                set errorcode 1
                set errormessage "-from specified without preceding -done"
                break
            }
            incr i
            if {$i == $argc} {
                set errorcode 1
                set errormessage "No from date"
                break
            }
            catch {set startDate [dateStringToExcelSerialDate [lindex $argv $i]]} 

            if {$startDate < 0} {
                set errorcode 1
                set startDate -1
                set errormessage "Badly formed from date"
                break
            }
            
        } elseif {$arg == "-to"} {

            if {$donemode == 0} {
                set errorcode 1
                set errormessage "-to specified without preceding -done"
                break
            }
            incr i
            if {$i == $argc} {
                set errorcode 1
                set errormessage "No to date"
                break
            }
            catch {set endDate [dateStringToExcelSerialDate [lindex $argv $i]]} dateError

            if {$endDate < 0} {
                set errorcode 1
                set endDate -1
                set errormessage "Badly formed to date"
                break
            }
            
        } elseif {$arg == "-datespan"} {

            if {$donemode == 0} {
                set errorcode 1
                set errormessage "-datespan specified without preceding -done"
                break
            } elseif {$startDate == -1} {
                set errorcode 1
                set errormessage "-datespan specified without preceding -from date"
                break
            }
            incr i
            if {$i == $argc} {
                set errorcode 1
                set errormessage "No datespan specified"
                break
            }
            set nscan [scan [lindex $argv $i] "%d" span]

            if {$nscan != 1} {
                set errorcode 1
                set errormessage "Badly formed span value"
                break
            }

            set endDate [expr {$startDate + $span}]

        } elseif {$arg == "-dated"} {

            if {$donemode == 0} {
                set errorcode 1
                set errormessage "-dated specified without preceding -done"
                break
            }

            set dated 1
        } elseif {$arg == "-undated"} {

            if {$donemode == 0} {
                set errorcode 1
                set errormessage "-undated specified without preceding -done"
                break
            }

            set undated 1
        } elseif {$arg == "-n"} {
            
            if {$donemode == 0} {
                set errorcode 1
                set errormessage "-n specified without preceding -done"
                break
            }

            set lineNumbers 1

        } elseif {$arg == "-i"} {
            incr i
            if {$i == $argc} {
                set errorcode 1
                set errormessage "No input filename"
                break
            }
            set infilename [lindex $argv $i]
        } elseif {$arg == "-o"} {
            incr i
            if {$i == $argc} {
                set errorcode 1
                set errormessage "No output filename"
                break
            }
            set outfilename [lindex $argv $i]
        } else {
            set errorcode 1
            set errormessage "Unknown option $arg"
            break
        }
    }

    # Validate
    if {$errorcode == 0} {
        # more than one mode specified
        set nModes [expr {$org2tdlmode + $tdl2orgmode + $testmode + $donemode}]
        if {$nModes > 1} {
            set errorcode 1
            set errormessage "Multiple modes selected, aborting."
        } elseif {$nModes == 0} {
            set errorcode 1
            set errormessage "No action selected."
        } elseif {($startDate > $endDate) && ($endDate != -1)} {
            set errorcode 1
            set errormessage "Start Date > End Date"
        }
    }

    # TODO: handle "-done -dated -undated"
    if {$donemode} {
        if {($startDate > 0)||($endDate > 0)} {
            # user set dates, therefore looking for dated items
            set dated 1

            if {$undated} {
                set errorcode 1
                set errormessage "Cannot select both dated and undated items"
            }
        } elseif {$dated} {
            # user explicitly asked for dated items
            set undated 0
        } elseif {$undated} {
            # user explicitly asked for undated
            set dated 0
        } else {
            # default done mode : dated and undated
            set undated 1
            set dated 1
        }
    }

    return [list $errorcode $errormessage $org2tdlmode $tdl2orgmode $testmode \
                $donemode $dated $undated $startDate $endDate \
                $infilename $outfilename $lineNumbers]
}

#########################################################################
#
# Main entry point.
#

# get the arguments
foreach \
    {errorcode errormessage org2tdlmode tdl2orgmode testmode donemode \
         dated undated startDate endDate infile outfile lineNumbers} \
    [parseArgs $argv] {}

# handle argument errors
if {$errorcode} {
    puts stderr $errormessage
    putUsage stderr
    exit 1
}

# execute the desired command
if {$testmode} {
    runTests $tests {} 0
} elseif {$org2tdlmode} {
    set errorcode [Org2TDL $infile $outfile]
} elseif {$tdl2orgmode} {
    set errorcode [TDL2Org $infile $outfile]
} elseif {$donemode} {
    # puts "startDate = $startDate"
    set errorcode [DoneForFile $infile $outfile \
                   $dated $undated $startDate $endDate $lineNumbers]
}



