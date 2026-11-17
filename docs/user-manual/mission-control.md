# Mission Control

Mission Control lets Humbug work through an objective by itself. Instead of starting and coordinating each
conversation yourself, you describe an objective. A lead AI conversation plans the work, hands
independent parts to delegated AI conversations, and reports its progress back to one dashboard.
Approval requests collect in a **Decision Inbox**: each mission shows its own requests, along with
those from conversations outside any mission.

Mission Control is a tab, like your conversations and editors. Each mission is followed in one of
two modes, never both at once:

- **Autonomous**: the mission is driven from Mission Control. Its lead conversation has no tab of
  its own; you follow it on the dashboard, and Mission Control keeps it going.
- **Classic**: the mission's lead is an ordinary conversation tab that you drive yourself, as
  you would any conversation. Mission Control lists the mission but does not carry it on.

A new mission starts in Autonomous mode. See
[Switching a mission between Classic and Autonomous](#switching-a-mission-between-classic-and-autonomous).

---

## Opening Mission Control

Open it from **Mindspace → Mission Control**, with **Ctrl+Shift+M** (macOS: **Cmd+Shift+M**), or
from the Quick Switcher. If it is already open, this brings it to the front. Like any other tab, it
is restored when you reopen the mindspace.

Closing the tab does not stop anything. Missions keep running, and Mission Control keeps following
them until the mindspace is closed, so reopening the tab shows everything that happened meanwhile.

If movement on screen is uncomfortable or distracting, turn on **Reduce motion** at the top of
Mission Control. Everything is then drawn still and in its final state: the agents do not move,
their remarks appear whole instead of being typed out, status dots do not pulse, cards do not
fade in, the page jumps rather than glides, and a finished mission is not celebrated with
confetti. Nothing is hidden; only the movement goes. The choice is saved for the mindspace.

---

## Starting a mission

Type what you want to achieve in the box at the bottom (**Tell Humbug what you want to
achieve…**) and press **Ctrl+Enter** (macOS: **Cmd+Enter**) or click the send button (the paper plane), just as you
would in a conversation. **Enter** on its own starts a new line. You can also click one of the example objectives to
fill in the box.

The box works like a conversation's input box:

- Click the paperclip to attach a file. Text and code files are attached as they are, and PDF and Word (`.docx`)
  files as the text in them. You are asked first if a file is larger than 100 KB. Each attached file is shown
  above the box; click its **×** to remove it. The files go to the mission lead with your message, and you still
  need to type a message to send them.
- Click the cog to change the AI settings, such as the model. Before a mission starts, these are the settings its
  lead starts with. With a mission selected, they are its lead's settings, and apply to its next reply. As in a
  conversation, they last until Humbug closes; **Apply to all** also makes them the mindspace's default.

When you are guiding a mission, the box does not repeat the mission's name, since the mission is the one on
show. Before a mission starts, it is marked **New mission**.

Humbug then:

1. Records the mission.
2. Starts a new conversation, the **mission lead**, in Autonomous mode, so it has no tab. Its
   transcript is in the conversations sidebar with a name that starts with `Mission-`. While
   Mission Control is the current tab and the conversations sidebar is showing, the selected
   mission's lead conversation is highlighted there, just as a conversation tab highlights its own.
3. Sends the lead your request, together with instructions to work out first what you actually
   want. A simple question, or a request for a quick explanation or opinion that needs at most a
   brief look at a file or two, is simply answered: no plan is made and no agents are started, and
   the mission shows **Answered** with the reply in **Latest from the mission lead**. Any big task
   gets a plan, even when nothing will be changed: reviewing a pull request, a branch or several
   files, investigating a problem, researching a topic in depth, or changing files, running
   commands, or building or fixing something. The same happens when you ask for a plan. The lead
   then plans the work, reports the plan, decides details it reasonably can by itself, checks its
   results, and stops only when it truly needs you. When it cannot tell, it plans if answering would
   take more than a brief look, and otherwise answers.

The lead works this out for every message you send, not only the first, so you can ask it questions
about a mission at any time. A question asked while a mission is running is answered and the work
carries on.

---

## How a mission keeps going

A mission in Autonomous mode carries on by itself. Each time the lead finishes a reply while steps in its plan are
still unfinished, Mission Control waits a moment and then asks it to continue with the next step.

A mission is done when your goal is met, not when one plan runs out of steps. The lead may work in
parts: when every step of a plan is done but the lead has not reported your goal met, the plan is
kept as a finished phase under **Earlier results**, the mission card says **Next: planning what is
left of the goal**, and Mission Control asks the lead to plan the next part. Only when the lead
reports that everything you asked for is done is the mission shown as **Mission accomplished**. If
you stop a step yourself and the rest is done, the mission counts as finished, as you chose. Missions
started before this change are finished when their plan is, as before.
It does not ask when:

- a step is marked **Blocked**, because the lead has stopped to ask you something;
- an approval is waiting in the Decision Inbox;
- there is no plan, because the lead answered a question rather than taking on work;
- the mission is finished, stopped or paused;
- the lead's reply ended in an error, or you cancelled it.

If the lead is asked to continue three times without finishing a step, Mission Control stops
asking and the activity feed shows that it paused, so a mission that is going round in circles
does not run on indefinitely. Sending the lead a message, or the lead finishing a step, starts
the count again.

Carrying on by itself does not include approving anything: unless you turn on
[Auto-approve](#auto-approve), every action that changes files, runs commands or sends data still
waits for your decision.

The lead uses the AI model set in your mindspace settings.

---

### Work happens in the background

While a mission is in Autonomous mode, the conversations its lead hands work to open behind the tab
you are looking at. They never take the keyboard focus or bring their tab to the front, so you can
keep reading Mission Control, or work elsewhere, while the mission runs. Their tabs are still there
if you want them, and they close as usual when their work is done. Missions in Classic mode open
delegated conversations as conversation tabs always have.

## If Humbug closes during a mission

Mission Control remembers whether each mission was working. If Humbug closes, or the mindspace is
switched, while a mission is still working, the mission is marked **Interrupted** when you open the
mindspace again, with a short note explaining why.

Click **Resume** to carry on. Humbug opens the lead conversation if it is not already open and
tells the lead that it was interrupted, so that it first checks what was actually finished (an
action or a delegated task may have stopped part way) and then continues with the next unfinished
step. If it was interrupted before it had reported a plan, it is asked instead to carry on with your
request: to plan it first if it is a big task, or to give the answer if that is all it needs. From there the mission carries on by itself as usual. You can also send the lead a message
instead, which resumes the mission too, or click **Stop** to leave it stopped.

To have interrupted missions pick up without clicking **Resume**, turn on **Auto-resume after
restart** at the top of Mission Control. This is saved for the mindspace and is off until you turn
it on. Interrupted missions are then resumed as soon as the mindspace opens, whether or not the
Mission Control tab is open.

A mission that was waiting for you when Humbug closed (asking a question, paused, or finished) is
not interrupted and is shown as it was. Anything the lead was in the middle of when Humbug closed,
such as an approval it was waiting for, is not carried over; resuming starts a new reply.

---

## The mission dashboard

When a mission is selected, Mission Control shows, from the top:

- **A reminder when something needs you**: while requests wait in the Decision Inbox, a banner
  says how many, and **Review now** takes you to the oldest one.
- **Working on your message**: from the moment you send the lead a message, including a follow-up
  to a mission that is already done, until the lead replies to it, a strip at the top names your
  message and says it is being worked on. It is not shown if the lead's reply fails or the mission
  is stopped or interrupted; the mission card says so instead.
- **The mission card** with the mission's title, summary, status and progress. Progress counts a
  task in progress as half done. Under the title, a journey bar shows where the mission is on its
  way: **You asked**, **AI plans it**, **AI team works** and **Job done**. The current stage glows,
  and while the AI is working a small moon circles it and a soft pulse spreads out from it; it turns amber when the mission is waiting for you. While the
  team works, the track beyond **AI team works** fills with the share of the plan that is done.
  Below it, a line gives what the lead is doing right now, in its own words. While the mission is
  still going, a green line under that says how far it has got and what comes next, such as
  **Step 2 of 4 done: Write the user guide · Next: Add a changelog**; after a follow-up starts new
  work, it names the work finished before. A finished step is never shown as the mission being
  done. Under the
  **Auto-approve** choice, a sentence says what the chosen level means.
- **At a glance**: four lines that sum the mission up: what is happening **Now**, what is
  **Holding it up** (your decision on a request, a pause, a blocked step, a failed reply, an
  interruption or a stop, or **Nothing**), how many requests are **Waiting for you**, and what it
  **Last finished**. **Hide details** folds away the figures, the plan, the agents, the conversation,
  earlier results and recent activity, leaving the mission card, this summary, the lead's latest
  message and the Decision Inbox; **Show details** brings them back. Classic missions do not have it.
- **Mission accomplished**: when every step is done and nothing you sent is still waiting for a
  reply, a report card appears at the top of the dashboard. A message you send to a finished
  mission makes it **Running** again until the lead has answered it. If you watched the mission
  finish, it is celebrated with a burst of confetti, once: opening it later, or the lead answering
  a follow-up, does not celebrate it again. It shows the lead's final report, how many steps were done and
  how many agents took part, with **Ask a follow-up** (to continue the same mission) and
  **New mission**. While the report is showing, the lead's last message is not repeated in
  **Latest from the mission lead**. If you then ask a question that the lead just answers, the report
  stays as it was and the answer is shown in **Latest from the mission lead**.
- **Earlier results**: once you have asked a follow-up, the rounds that finished before it are
  kept here, newest first, so the new work never hides what came before. Each round shows when it
  finished, how many of its steps were done, what you asked for (your objective, or the follow-up
  that started it) and the first line of the lead's final report. Click a round to read the whole
  report, and click it again to fold it away. Earlier results are kept with the mission when
  Humbug closes.
- **Latest from the mission lead**: the lead's most recent reply in full, without its plan. The card
  is always just as tall as the reply, growing for a long one and shrinking for a short one. This is where
  the lead reports progress and asks you questions. When it has stopped to wait for you, the card
  is highlighted and marked **Waiting for your reply**: click **Reply**, or type in the box at the
  bottom, to answer it. The **Reply** button only appears when the lead is waiting for you.
- **Statistics**: tasks in the plan, tasks completed, tasks in progress, approvals waiting for
  you, and agents currently working. Click **Completed** or **In progress** to show only those
  steps in the plan, and click it again (or **Show all**) to see every step. Click the approvals
  or agents figure to jump to the Decision Inbox or the Agents list.
- **Agents**: a live picture of who is working on the mission, shown above the Mission Plan. You are at the top, the lead
  below you, and each delegated agent below the agent that handed it the work. Every agent has a
  short name of its own (such as Atlas or Nova) that it keeps for the life of the mission, and is
  described by the plan step it is working on where the lead has said so. Each is drawn as a small
  robot that shows what it is doing: it types with a spinning gear while using a tool, shows a
  thought bubble while thinking, an amber **!** while waiting for your approval, and dozes while
  idle. Particles flow along the arrows to agents that are working, and the arrow to an agent
  waiting for you turns into an amber dashed line. Each agent also tells you what it is doing in
  a speech bubble, typed out as it speaks: for example "Writing intro.md for you…", "Bringing in a
  helper agent for this…" or, when it is done, "All done! Anything else I can do for you?". A new
  agent introduces itself once; after its first step, while it works out its next one, it says what
  it has just done and how many actions it has taken so far, such as "Read merge.py, now working out
  what's next… (2 actions so far)". A tag under each agent's name shows the AI model it uses, such
  as **Claude Sonnet 5.5**, and the same model is named in **Show as a list** and when you hover over
  the agent. An
  agent waiting for your approval says what it wants to do and shows **Approve**, **Unsure** and
  **Reject** buttons in its bubble, which work exactly like the same buttons in the Decision Inbox.
  Answering in the bubble leaves you where you are: the page does not move, even as the banner
  and cards above change. The bubble gives the request in a line, and **Read more** is the one
  button that takes you to the request in the Decision Inbox, gliding there and highlighting it,
  where it is shown in full. A destructive request is not approved from the bubble: it shows
  **Review**, **Unsure** and **Reject**, and **Review** takes you to the request in the Decision
  Inbox, where you can see what it acts on and whether it can be undone before you approve it.
  Once you approve a delegation, the agent that
  asked says it is waiting for its helper to finish and report back, for as long as the helper
  works. An agent that is
  pausing on purpose, for example with the clock tool's sleep, shows a ring and a bar that fill as
  the pause runs down, the time left, and a **Finish waiting** button that ends the pause early so
  the agent carries on straight away. Clicking an agent, or the conversation named on a plan step
  or a request, opens that conversation in a column beside Mission Control, so Mission Control
  stays in view. Hover over a robot for a summary, or click it to
  open its conversation. The picture can also be used without a mouse: tab to it, then use the
  arrow keys to move between agents, **Home** and **End** to jump to the ends, **Enter** to open
  the highlighted agent's conversation, and **Esc** to drop the highlight. The same summary the
  hover shows is given for the highlighted agent. **Tab** moves on from the picture to the next
  control, as anywhere else. For screen readers, the picture is described in words: every agent
  with what it is doing, the highlighted one first, announced as the highlight moves.
  **Show as a list** under the picture lists the same agents and what each
  is doing, and **Hide the list** folds it away again. The
  same names are used in the Decision Inbox and Recent Activity.
- **Mission Plan**: the steps the lead has reported, each marked Pending, In progress, Completed
  or Blocked. Click a step to open it and see the lead's latest progress note, the evidence for a
  completed step, and the delegated conversation working on it, with a button to open that
  conversation. Click the step again to close it. What a step shows is only as complete as the
  lead's report: a step the lead has said nothing about shows that no progress has been
  reported.
- **Recent Activity**: a tall scrolling feed, newest first, of what has happened in this mission
  since Mission Control was first opened in this mindspace. It includes the objective you started
  it with, each message you sent from Mission Control, and anything you typed directly into one of
  the mission's conversations. Entries marked **▶** have more to show: click one to open it and see
  the whole story, such as your full message, the full approval request, the file or command a tool
  worked on, or the reviewer's reasoning, and click it again to close it.
- **Conversation**: in the right-hand column, below Recent Activity. It shows everything you and the
  lead have said, oldest first, as you would read it in a conversation tab but without the input box (you reply from the box at
  the bottom of Mission Control). Your messages sit on the right, shown exactly as you typed them;
  the lead's replies are on the left under its name, formatted and in full. Beside the name, a tag
  shows the AI model that wrote that reply, such as **DeepSeek V4.1 Flash**. Between them, a small
  **⚙** line names each tool the lead used and what it worked on, such as
  `filesystem.write_file · notes.md`. Your first message is shown as the request you typed, not the
  instructions Mission Control adds to it, and Mission Control's own requests for the lead to carry
  on are left out. The count beside the heading is how many messages you have sent. Each message is
  always as tall as its text. The conversation takes only the room it needs, up to a set height;
  beyond that it scrolls inside the card, with the newest message in view unless you have scrolled up
  to read an earlier one.

Use the chips above the dashboard to switch between missions, or **+ New mission** to start
another. **+ New mission** always stays in view. When there are more missions than fit, the chips
scroll sideways: use the **‹** and **›** arrows, swipe sideways on a trackpad or touch screen, or
scroll sideways with a mouse that can. The faded end shows which way more missions lie, and the
selected mission is always scrolled into view.

A mission waiting for your decision shows a pulsing amber count on its chip. When another mission
than the one you are looking at is waiting, a banner under the chips names it (or says how many
are waiting), and **Go there** opens it.

Whenever Mission Control moves the page for you, such as **Review now**, **Read more**, a
statistic, opening a plan step or the plan history, or opening an activity entry, it glides there
rather than jumping, so you can see where you have been taken. Each chip has a **⋯** button (you can also right-click the chip) with:

- **Rename**, to give the mission a name of your own. The name stays, even when the lead later
  reports a plan with a different title.
- **Delete**, which removes the mission from Mission Control after you confirm. Its conversations
  are kept.

### Mission status

| Status | Meaning |
|---|---|
| **Planning** | The lead is working and has no unfinished plan yet: it is planning the mission, a follow-up you sent after it finished, or the next part of your goal |
| **Running** | The lead or a delegated agent is working through the plan, including while a request is being approved automatically and in the moment before the lead is asked to carry on |
| **Needs you** | A conversation in the mission is waiting for your approval |
| **Waiting for you** | Nothing is running; the lead may have asked you a question |
| **Answered** | The lead answered your question; no work was needed, so there is no plan |
| **Failed** | The lead's latest reply ended in an error it could not recover from. The mission card says so, with **Retry** to send the same request to the lead again (the error and any partial reply are removed first), and **Open the lead's conversation** to see why. Sending a message also tries again. **Resume** is offered only to an **Interrupted** mission |
| **Completed** | The lead reports that everything you asked for is done, and every task in its plan is completed |
| **Paused** | You paused the mission: nothing is approved automatically and the lead does not carry on by itself until you click **Continue** |
| **Stopped** | You stopped the mission |

### Mission actions

**Pause** and **Stop** are in the mission card. **Switch to Classic mode** and **Delete** are in the **⋯** menu
beside them, so a mission cannot be deleted by a slip of the mouse aimed at **Stop**.

- **Pause** lets whatever is running now finish, then holds the mission so you can look at the
  work. While it is paused, nothing is approved automatically, whatever its auto-approve level:
  every request waits in the Decision Inbox. The approval a conversation may hold for file
  changes for the rest of its turn is withdrawn, and the lead is not asked to continue. A
  paused mission stays paused if Humbug restarts, and **Auto-resume after restart** leaves it
  alone. **Continue** lifts the pause: requests still waiting are considered again under the
  mission's auto-approve level, and the lead is asked to carry on if there is work left.
  Messages you send while it is paused are delivered as usual.
- **Switch to Classic mode** opens the lead conversation as a tab beside Mission Control and
  moves the mission to Classic mode.
- **Stop** cancels the lead and every delegated conversation in the mission, as **Esc** does in
  a conversation tab. Any approval they were waiting for is rejected. Stopping a paused mission
  ends the pause.
- **Delete** removes the mission from Mission Control after you confirm. Its conversations are kept.

When a mission is selected, the box at the bottom sends your message to its lead instead of
starting a new mission. Sending a message to a stopped mission resumes it.

### Stopping a single task

Open a step in the Mission Plan and click **Stop task** to stop just that task while the rest of
the mission carries on. The step is marked **Stopped** and stays stopped even if the lead reports
it differently later. If a delegated conversation was working on the task and is open, it is
stopped too, and the lead is told not to do any more work on the task. When the delegate cannot
be found, the activity feed says the task was stopped but its delegate may still be running. A
mission whose remaining tasks are all completed or stopped counts as completed.

### Plan history

When a mission's plan is finished, the lead often suggests what to do next. If you ask it to
carry on (for example, by replying "yes"), or the lead starts new work itself, the finished plan
moves to the mission's history. As soon as the lead starts planning the new work, the Mission Plan
shows the finished plan under **History**, says the lead is drafting the new plan, and progress
starts again from 0%. The same happens when a finished plan did not reach your goal and the lead
plans what is left. If you only ask a question, the lead answers it and the finished plan comes
back as the mission's plan, as it was.

Click **History** in the Mission Plan heading to see the plans the mission finished earlier, newest
first, each with its title, how many steps were done and when it finished. Click a plan to see its
steps and the evidence the lead reported for them, and click it again to close it. **History** only
appears once a mission has an earlier plan, and the history is kept with the mission when Humbug
closes. The lead's final report on each finished plan, and what you asked for, are shown under
**Earlier results** on the dashboard.

---

## Switching a mission between Classic and Autonomous

There are three ways to switch:

- In Mission Control, **Switch to Classic mode** in the **⋯** menu of the mission card, or
  **Switch to Autonomous mode** on a Classic mission.
- Right-click the mission's conversation in the conversations sidebar and choose
  **Switch to Classic mode** or **Switch to Autonomous mode**, whichever it is not in.
- Right-click the tab of a Classic mission's conversation and choose
  **Switch to Autonomous mode**.

Switching to Classic opens the conversation as a tab without interrupting it: if the lead is in
the middle of a reply, the tab carries on showing it. From then on the conversation is yours to
drive and Mission Control no longer asks it to continue; the dashboard shows a short note with a
button to switch back.

Switching to Autonomous closes the conversation's tab and hands the mission back to Mission
Control, which carries on with any unfinished steps. Because closing the tab would cut off a reply
in progress, a conversation that is busy (replying, or waiting for an approval) is not switched:
wait for it to finish, or stop it, then switch.

Opening an Autonomous mission's conversation from the conversations sidebar in the usual way also
moves the mission to Classic mode, so the two are never followed in parallel. If the lead was in
the middle of a reply when you opened it, that reply is stopped and the tab shows the
conversation as it was last saved.

---

## Auto-approve

By default every action that changes files, runs commands or sends data waits for you. For a
mission in Autonomous mode you can let an **AI reviewer** make some of those decisions, or, with
**Everything**, let the mission run end to end without stopping for approvals at all. Choose a
level with **Auto-approve** on the new-mission screen before you start, so it applies from the
lead's very first request, or in the mission header at any time afterwards. Each new mission
starts at **Off** unless you choose otherwise:

| Level | What is approved without you |
|---|---|
| **Off** (the default) | Nothing. Every request comes to you. |
| **Safe actions** | File changes the journal records that Humbug does not mark destructive, approved straight away. Other requests Humbug does not mark destructive, such as delegating to a helper, go to the reviewer. Deleting, moving, overwriting, terminal commands and network writes always come to you. |
| **Everything** | Every request, approved straight away without a review, so nothing waits for you. Humbug asks you to confirm before turning this on. |

**File changes the journal records** are not sent to the reviewer, because the journal keeps what
the file held before, so the change can usually be reversed. At **Safe actions**, each such change
that Humbug does not mark destructive is approved as it comes. At **Everything**, Mission Control
approves the first one in an agent's turn and turns on that conversation's own **Auto-approve file
changes until your next message** switch, the same one you can turn on from an approval card in a
conversation (see [AI tools](ai-tools.md)), so the agent's other changes in that turn go ahead
without stopping. The switch turns itself off when the turn ends, so each turn's first change is
checked against the level again, and choosing **Safe actions** or **Off** turns it off straight
away. Each of these approvals shows in Recent Activity as **Approved … — recorded in the journal**.

Recorded is not the same as guaranteed to be reversible. The journal only covers files inside the
mindspace, outside `.humbug`, and below its size limit, and reversing a file does not undo what has
already happened because of it, such as a file that was run or committed. At **Safe actions**,
anything the journal does not record, such as moving a folder of files, follows the reviewer rules
below. At **Everything** it is approved like any other request, and shows in Recent Activity as
**Approved … — auto-approve is set to Everything**.

The reviewer is used only at **Safe actions**. For each request in scope, the reviewer, using the same AI model as the mission's lead, is given
the request together with what you have asked for: the mission's original objective, the messages
you have sent the lead since (which refine or replace the objective), the plan, and the lead's
latest update. It approves routine work in your project that the current step needs, and leaves
the request for you if it does not fit what you asked for, reaches beyond it, or could expose
secrets or send data somewhere unexpected.

Humbug treats many everyday edits as destructive: overwriting an existing file, applying a diff,
moving or deleting files, and any terminal command. With **Safe actions** these still come to you;
choose **Everything** to have them approved too. A request that still needs you because
of the level, or because the mission is in Classic mode, says so in the Decision Inbox.
While it decides, the request shows **AI reviewing…** in the Decision Inbox, and you can still
answer it yourself. If the reviewer is unsure, cannot be reached, or replies in a way Humbug cannot
read, the request stays in the inbox for you with the reviewer's reason.

You can change the level at any time, including while a request is waiting. Each request in the
inbox is then looked at again under the new level. For example, a destructive request that came to
you under **Safe actions**, or a request the reviewer left for you, is approved as soon as you
choose **Everything**. Choosing **Off** stops any further automatic approvals;
waiting requests stay with you.

**Everything** is an approval you give in advance, for one mission, so Humbug asks you to confirm
it and says what it covers: every request, including deleting, moving and overwriting files,
terminal commands (which cannot be limited to particular files), delegating to helpers and
sending data over the network. It lasts only while that mission runs in Autonomous mode.
Stopping the mission, or switching it to Classic mode, turns it back to **Off** straight away, and
switching back to Autonomous does not turn it on again: choose it again if you want it. Humbug
saves when you confirmed it; a mission whose confirmation was not saved comes back at **Off** when
Humbug starts. Your confirmation (with the mission, the time and what it covers), every approval
made under it, and the moment it ends are all recorded in the [Mindspace Log](mindspace-log.md).

While the mission you are looking at is at **Everything**, a red banner under the Mission Control
title says so. It stays in view however far you scroll, and its **Turn off** button sets the
mission back to **Off** at once.

Every automatic approval, and every request the reviewer leaves for you, is recorded in the
[Mindspace Log](mindspace-log.md), with the reviewer's reasoning where there was a review, and the reviewer's token use is
counted in [Token Usage](token-usage.md). Auto-approve applies only while a mission is in
Autonomous mode.

The reviewer is itself an AI and can be wrong. Text it reviews, such as file contents or web pages
quoted in a request, may try to persuade it; it is told to treat that as data, but this cannot be
guaranteed. **Everything** has no reviewer at all: the mission's agents can delete files, run any
terminal command and send data over the network without anyone checking. Use it only for work you
are comfortable not supervising.

---

## The Decision Inbox

The Decision Inbox lists the tool approvals waiting for the mission you are looking at, together
with those from conversations that are not part of any mission. Requests from other missions are
not mixed in: they show on their own mission's page, and the chip and banner described above tell
you they are waiting. Each request shows the tool, the reason it gave, any
detail such as a diff, which conversation asked, and whether the operation is destructive.
Where the request names what it acts on (a file, a move from one path to another, a command or a
URL), that is shown as **Acts on**. A destructive request also says, before you approve it,
whether it can be undone: a file change that Humbug's file change journal records can be, and
anything else, such as a terminal command or a network request, cannot be undone by Humbug.

- **Approve** allows the operation. It is red for destructive operations.
- **Unsure** declines the operation and asks the conversation to discuss it with you.
- **Reject** declines the operation.

These are the same decisions as the buttons in the conversation tab, and they are recorded in the
[Mindspace Log](mindspace-log.md) in the same way. Whichever you answer first wins: the request
disappears from the other place. A request that has already been answered elsewhere cannot be
answered again.

---

## What Mission Control does not do

Mission Control is a first version, and it is deliberately limited.

- **Nothing is approved without you unless you turn on auto-approve.** With auto-approve off,
  every action that changes files, runs commands or sends data waits for your decision, exactly as
  it does in a conversation tab. With it on, see [Auto-approve](#auto-approve) for what the AI
  reviewer may decide and its limits.
- **The plan is the lead's own report.** Mission Control shows what the lead AI says about its
  progress; it does not check that work itself. Read the lead's summary and the results before
  relying on them.
- **Resuming relies on the lead checking its own work.** After an interruption the lead is asked
  to confirm what was finished, but Mission Control cannot tell on its own whether an action
  completed before Humbug closed.
- **Agents are only shown while their conversations are open.** A delegated conversation appears
  under Agents while it is open; Humbug closes it when its task finishes. Delegated conversations
  that were already open when the mindspace was reopened are not linked back to their mission.
- **Recent Activity is not kept.** It starts when Mission Control is first opened and is cleared
  when the mindspace closes. The [Mindspace Log](mindspace-log.md) is the permanent record.
- **A renamed lead conversation is no longer linked to its mission.**

- **Delegated conversations still appear as tabs while they work.** In Autonomous mode the lead
  has no tab, but the conversations it delegates to open as tabs as they always do, and close on
  their own when their task is finished.
- **Stopping a task relies on the delegate being identifiable.** If the lead did not record which
  delegated conversation was working on a task, stopping the task tells the lead to stop but
  cannot stop that conversation directly.

Missions are stored in `.humbug/missions/` within the mindspace. Like everything else in
`.humbug/`, they are outside what the AI's file tools can reach.

---

*[Index](index.md) · Previous: [Token Usage](token-usage.md) · Next: [User Settings](user-settings.md)*
