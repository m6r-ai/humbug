# Mission Control

Mission Control lets Humbug work through an objective by itself. Instead of starting and coordinating each
conversation yourself, you describe an objective. A lead AI conversation plans the work, hands
independent parts to delegated AI conversations, and reports its progress back to one dashboard.
Every approval request from every open conversation collects in a single **Decision Inbox**.

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

---

## Starting a mission

Type what you want to achieve in the box at the bottom (**Tell Humbug what you want to
achieve…**) and press **Ctrl+Enter** (macOS: **Cmd+Enter**) or click **Send**, just as you
would in a conversation. **Enter** on its own starts a new line. You can also click one of the example objectives to
fill in the box.

Humbug then:

1. Records the mission.
2. Starts a new conversation, the **mission lead**, in Autonomous mode, so it has no tab. Its
   transcript is in the conversations sidebar with a name that starts with `Mission-`.
3. Sends the lead your objective, together with instructions to plan the work, report the plan,
   decide details it reasonably can by itself, check its results, and stop only when it truly
   needs you.

---

## How a mission keeps going

A mission in Autonomous mode carries on by itself. Each time the lead finishes a reply while steps in its plan are
still unfinished, Mission Control waits a moment and then asks it to continue with the next step.
It does not ask when:

- a step is marked **Blocked**, because the lead has stopped to ask you something;
- an approval is waiting in the Decision Inbox;
- the mission is finished or stopped;
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
step. From there the mission carries on by itself as usual. You can also send the lead a message
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

When a mission is selected, Mission Control shows:

- **The mission card** with the mission's title, summary, status and progress. Progress counts a
  task in progress as half done.
- **Latest from the mission lead**: the lead's most recent reply, without its plan. This is where
  the lead reports progress and asks you questions. When it has stopped to wait for you, the card
  is highlighted and marked **Waiting for your reply**: click **Reply**, or type in the box at the
  bottom, to answer it. The **Reply** button only appears when the lead is waiting for you.
- **Statistics**: tasks in the plan, tasks completed, tasks in progress, approvals waiting for
  you, and agents currently working. Click **Completed** or **In progress** to show only those
  steps in the plan, and click it again (or **Show all**) to see every step. Click the approvals
  or agents figure to jump to the Decision Inbox or the Agents list.
- **Mission Plan**: the steps the lead has reported, each marked Pending, In progress, Completed
  or Blocked. Click a step to open it and see the lead's latest progress note, the evidence for a
  completed step, and the delegated conversation working on it, with a button to open that
  conversation. Click the step again to close it. What a step shows is only as complete as the
  lead's report: a step the lead has said nothing about shows that no progress has been
  reported.
- **Agents**: a live picture of who is working on the mission. You are at the top, the lead
  below you, and each delegated agent below the agent that handed it the work. Every agent has a
  short name of its own (such as Atlas or Nova) that it keeps for the life of the mission, and is
  described by the plan step it is working on where the lead has said so. Each is drawn as a small
  robot that shows what it is doing: it types with a spinning gear while using a tool, shows a
  thought bubble while thinking, an amber **!** while waiting for your approval, and dozes while
  idle. Particles flow along the arrows to agents that are working, and the arrow to an agent
  waiting for you turns into an amber dashed line. Each agent also tells you what it is doing in
  a speech bubble, typed out as it speaks: for example "Writing intro.md for you…", "Bringing in a
  helper agent for this…" or, when it is done, "All done! Anything else I can do for you?". An
  agent waiting for your approval says what it wants to do and shows **Approve** and **Reject**
  buttons in its bubble, which work exactly like the same buttons in the Decision Inbox; the
  bubble gives the request in a line, and the Decision Inbox shows it in full. An agent that is
  pausing on purpose, for example with the clock tool's sleep, shows a ring and a bar that fill as
  the pause runs down, the time left, and a **Finish waiting** button that ends the pause early so
  the agent carries on straight away. Clicking an agent, or the conversation named on a plan step
  or a request, opens that conversation in a column beside Mission Control, so Mission Control
  stays in view. Hover over a robot for a summary, or click it to
  open its conversation. A list of the same agents and what each is doing follows the picture. The
  same names are used in the Decision Inbox and Recent Activity.
- **Recent Activity**: a scrolling feed, newest first, of what has happened in this mission since Mission Control was first opened
  in this mindspace, including the objective you started it with and each message you sent it.

Use the chips above the dashboard to switch between missions, or **+ New mission** to start
another. Each chip has a **⋯** button (you can also right-click the chip) with:

- **Rename**, to give the mission a name of your own. The name stays, even when the lead later
  reports a plan with a different title.
- **Delete**, which removes the mission from Mission Control after you confirm. Its conversations
  are kept.

### Mission status

| Status | Meaning |
|---|---|
| **Planning** | The lead is working and has not reported a plan yet |
| **Running** | The lead or a delegated agent is working |
| **Needs you** | A conversation in the mission is waiting for your approval |
| **Waiting for you** | Nothing is running; the lead may have asked you a question |
| **Completed** | Every task in the plan is completed |
| **Stopped** | You stopped the mission |

### Mission actions

- **Switch to Classic mode** opens the lead conversation as a tab beside Mission Control and
  moves the mission to Classic mode.
- **Stop** cancels the lead and every delegated conversation in the mission, as **Esc** does in
  a conversation tab. Any approval they were waiting for is rejected.
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
moves to the mission's history and the new work starts with an empty plan. Progress starts again
from zero and the status shows the new work, not the finished plan. Until the lead reports its new
plan, the Mission Plan says that it is being drafted.

Click **History** in the Mission Plan heading to see the plans the mission finished earlier, newest
first, each with its title, how many steps were done and when it finished. Click a plan to see its
steps and the evidence the lead reported for them, and click it again to close it. **History** only
appears once a mission has an earlier plan, and the history is kept with the mission when Humbug
closes.

---

## Switching a mission between Classic and Autonomous

There are three ways to switch:

- In Mission Control, **Switch to Classic mode** in the mission header, or
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
mission in Autonomous mode you can let an **AI reviewer** make some of those decisions instead.
Choose a level with **Auto-approve** in the mission header:

| Level | What the reviewer may approve |
|---|---|
| **Off** (the default) | Nothing. Every request comes to you. |
| **Safe actions** | Requests Humbug does not mark destructive, such as creating or writing files and delegating to a helper. Deleting, moving, overwriting, terminal commands and network writes always come to you. |
| **Everything** | Any request, including destructive ones. Humbug asks you to confirm before turning this on. |

For each request in scope, the reviewer, using the same AI model as the mission's lead, is given
the request together with what you have asked for: the mission's original objective, the messages
you have sent the lead since (which refine or replace the objective), the plan, and the lead's
latest update. It approves routine work in your project that the current step needs, and leaves
the request for you if it does not fit what you asked for, reaches beyond it, or could expose
secrets or send data somewhere unexpected.

Humbug treats many everyday edits as destructive: overwriting an existing file, applying a diff,
moving or deleting files, and any terminal command. With **Safe actions** these still come to you;
choose **Everything** to let the reviewer decide them too. A request that still needs you because
of the level, or because the mission is in Classic mode, says so in the Decision Inbox.
While it decides, the request shows **AI reviewing…** in the Decision Inbox, and you can still
answer it yourself. If the reviewer is unsure, cannot be reached, or replies in a way Humbug cannot
read, the request stays in the inbox for you with the reviewer's reason.

You can change the level at any time, including while a request is waiting. Each request in the
inbox is then looked at again under the new level. For example, a destructive request that came to
you under **Safe actions** is sent to the reviewer when you choose **Everything**, and a request the
reviewer left for you is reviewed again. Choosing **Off** stops any further automatic approvals;
waiting requests stay with you.

Every automatic approval, and every request the reviewer leaves for you, is recorded in the
[Mindspace Log](mindspace-log.md) with the reviewer's reasoning, and the reviewer's token use is
counted in [Token Usage](token-usage.md). Auto-approve applies only while a mission is in
Autonomous mode.

The reviewer is itself an AI and can be wrong. Text it reviews, such as file contents or web pages
quoted in a request, may try to persuade it; it is told to treat that as data, but this cannot be
guaranteed. Use **Everything** only for work you are comfortable not supervising.

---

## The Decision Inbox

The Decision Inbox lists every tool approval waiting in the mindspace, from any open
conversation, not only from missions. Each request shows the tool, the reason it gave, any
detail such as a diff, which conversation asked, and whether the operation is destructive.

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
