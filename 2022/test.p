DEFINE VARIABLE cMessage          AS CHARACTER NO-UNDO.
DEFINE VARIABLE isFridayAfternoon AS LOGICAL NO-UNDO.

RUN checkDay(TODAY, OUTPUT isFridayAfternoon).

FIND FIRST Backlog.Tickets NO-LOCK
WHERE Backlog.Tickets.Complexity = "High"
  AND Backlog.Tickets.Deadline   = "ASAP"
   BY Backlog.Tickets.Priority.

IF isFridayAfternoon AND (NOT AVAIL Backlog.Tickets) THEN RUN prGoToHappyHour(OUTPUT cMessage).
ELSE RUN prSolveItWithOpenEdge(Backlog.Tickets.Id, OUTPUT cMessage).

MESSAGE cMessage SKIP cDeveloper
VIEW-AS ALERT-BOX "Info".
