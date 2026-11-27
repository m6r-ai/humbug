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

Each mission has a tab of its own, named after the mission, so you can watch two missions side
by side by putting their tabs in different columns. Missions keep working whether or not their tab
is open, and however many are running at once.

To start a mission, open a new-mission tab from **Mindspace → Mission Control**, with
**Ctrl+Shift+M** (macOS: **Cmd+Shift+M**), from the Quick Switcher, or with **+** in the
**Missions** view of the left panel (the rocket icon on the rail). If a new-mission tab is already
open it is brought to the front rather than a second one opened. Once you send a request there,
that tab becomes the new mission's tab.

To open a mission, click it in the Missions view. As in the Conversations view, a single click
opens a temporary tab that the next one you open in that column replaces, and a double click opens
a tab that stays; double-click a temporary tab's label to keep it. A mission that already has a tab
is brought to the front instead. A notification, or the mission button on a conversation, opens the
mission it is about the same way. Like any other tab, a mission's tab is restored when you reopen
the mindspace.

When Humbug is set to Arabic, Mission Control reads from right to left: the send button, the
arrows on folded sections, the journey bar and the order of buttons all follow, and labels are
not letter-spaced. File names, code and diffs still read left to right.

Closing the tab does not stop anything. Missions keep running, and Mission Control keeps following
them until the mindspace is closed, so reopening the tab shows everything that happened meanwhile.

If movement on screen is uncomfortable or distracting, turn on **Reduce motion** at the top of
Mission Control. Everything is then drawn still and in its final state: the agents do not move,
their remarks appear whole instead of being typed out, status dots do not pulse, cards do not
fade in, the page jumps rather than glides, the picture on the new-mission screen is shown as
it ends, and a finished mission is not celebrated with confetti. Nothing is hidden; only the movement goes. The choice is saved for the mindspace, as a
[mindspace setting](mindspace-settings.md#mission-control) you can also change in **Settings**.

---

## Starting a mission

The first time you open the new-mission screen, a short guide explains Mission Control in three
steps: **Define your mission** (what happens to a request), **Watch it unfold** (how you follow
the work) and **Stay in control** (how you stay in charge of what it changes). **Got it** puts it away; the **How does Mission Control work?** link brings it
back. Putting it away is remembered for you, in every mindspace.

The new-mission screen opens with a short animation of Humbug's logo arriving. Two streams of
light fly in and meet, the logo turns into view out of the flash with rings of light circling it,
a sheen crosses it, and it comes to rest over a pedestal of light with its reflection on the
floor. It plays once, the first time you open the screen in a Mission Control tab; after that,
the picture rests on the logo. The round button in its top corner plays it again whenever you
like, even with **Reduce motion** on, since you asked to see it. The Decision Inbox is not shown
here while nothing is waiting, so the welcome has the whole width; if a conversation outside any
mission asks for an approval, the inbox appears beside it.

Type what you want to achieve in the box at the bottom (**Tell Humbug what you want to
achieve…**) and press **Ctrl+Enter** (macOS: **Cmd+Enter**) or click the send button (the paper plane), just as you
would in a conversation. **Enter** on its own starts a new line. Until you have run your first mission, the screen
also offers a few example objectives; click one to fill in the box.

Below the title, the screen keeps to what starting needs: the **Auto-approve** choice. Hover over
a level to read what it means. **Off** needs no explaining, so only **Safe actions** and
**Everything**, which approve requests on your behalf, say what they do in a line under the choice.

The options you choose once and rarely change are behind the cog at the right of the box: on the
new-mission screen it opens **New mission options**, with **Start every new mission at this level**,
**Let me review the plan before work starts**, **Add done checks (optional)**, **Limit the tokens
this mission can use**, **Save what you typed as a template**, and **AI settings…**. An option that is on shows as a small chip under the
**Auto-approve** choice, such as **Plan review ✕**; click the chip to turn the option off.

### Starting every mission at the same auto-approve level

New missions start with auto-approve **Off**. If you always want **Safe actions** for a project,
choose it on the new-mission screen and tick **Start every new mission at this level** in the
cog's **New mission options**; clear the tick, or click the **Always this level** chip, to go back
to Off. The choice is kept with the mindspace's settings. **Everything** can never
be kept this way: it has to be chosen, and confirmed, for each mission.

### Reviewing the plan first

Tick **Let me review the plan before work starts** in the cog's **New mission options**, and the lead drafts
its plan and then waits, without starting any task, delegating or changing anything. The mission
shows **Plan ready**, and Mission Control tells you, as it does when a decision is waiting. The
lead's speech bubble asks **Here's my plan. Shall I start?** with two buttons: **Yes, start** lets
the lead carry it out (as **Start work** in the mission card does), and **No** puts the cursor in
the box at the bottom, which now reads **Tell the lead what to change in the plan…**. A message
there asks the lead to change the plan: it reports the revised plan and waits again. **Stop** ends
the mission.

While the plan waits, you can change it yourself:

- **Move a step** by dragging it by its handle (⠿) to another place in the plan; a line shows
  where it will land. With a step selected, **Alt+Up** and **Alt+Down** move it without a mouse.
- **Skip a step** with **Skip** beside it. It is struck through and marked **Skipped**, and no
  longer counts towards the plan's progress or its number of steps. **Include** brings it back.

Once you have changed the plan the lead asks **You changed the plan. Shall I go ahead with the
changed plan?** **Yes, start** sends the lead the plan in your order with the skipped steps
marked, and the feed says **You approved the plan with your changes**. Mission Control keeps the
skipped steps skipped whatever the lead reports afterwards. Skipping a step is not the same as
stopping one: the mission is still accomplished only when the lead reports the goal met, and
your [done checks](#done-checks), if any, pass. If you ask the lead for a revised plan instead,
its new plan replaces your changes.

### Changing the plan while a mission is paused

Pause a mission and you can rearrange its plan the same way: drag a step by its handle, use
**Alt+Up** and **Alt+Down**, or **Skip** and **Include** it. Only steps that have not started can
be moved or skipped. A step that is under way or done keeps its place, has no handle and no
**Skip**, and a move that would shift it is not made. The lead says **You changed the plan. I'll
follow the changed plan when you resume.**

When you press **Continue** to end the pause, Mission Control sends the lead the plan in your order, with the
skipped steps marked, in place of its usual request to continue. If you send the lead a message
first, the changed plan goes with it. The lead is told once; after that it carries on from the
changed plan as from any other. As with a plan you change before approving it, it is the lead
that follows your order; Mission Control keeps skipped steps skipped whatever the lead reports.
If the lead reports a plan with different steps before you resume, its plan replaces your
changes. Only the mission's first plan is reviewed this way; once you have started the work, the
lead carries on as any mission does. The choice is kept with the mission, so a mission waiting
for you is still waiting after Humbug restarts.

The lead is asked to wait, and Mission Control does not ask it to continue until you start the
work, but it is the AI that holds back: every action that changes files or runs commands still
needs your approval, as always, if it tries anything before you have started the work.

### Done checks

Done checks are optional. Without them a mission is accomplished when its lead says everything
you asked for is done. With them, it is accomplished only when the lead reports that every check
passed, so you get evidence rather than the lead's word.

Tick **Add done checks (optional)** in the cog's **New mission options**, and a box for them appears on
the new-mission screen; write one check per line. A check
can be a command to run, such as `npm test passes` or `python -m tools.code_checker passes`, or
something to confirm, such as `POC/about.html is linked from the dashboard`. Leave the box ticked
but empty, or untick it, and the mission has no checks.

When the lead says the work is done, it runs every check and reports, for each one, whether it
passed and what it saw. The mission page shows them in a **Done Checks** card under the mission,
with how many have passed: each check is marked **Passed**, **Failed** or **Not run yet**, with the
lead's evidence beside it, such as `42 tests passed` or `dashboard.html has no link to about.html`.

If a check fails, or the lead says it is done without running them, the mission is not accomplished.
Mission Control sends the lead back with the checks that failed and what it saw, and the lead fixes
the work and runs every check again. The mission stays **Running** with **Next: getting the done
checks to pass**. After three tries that still leave a check failing, Mission Control stops sending
it back and shows **Looks stuck**: read the results, then send the lead a message to help, or
press **Carry on** to give it three more tries. A message you send while a check is failing is
taken as help with the checks, not as a new request.

What these checks can and cannot do:

- **The lead runs the checks and reports the results.** Mission Control does not run them itself.
  It holds the lead to reporting every check, passed, with evidence, before the mission counts as
  done, but the result is what the lead says it saw. Read the evidence, and open the lead's
  conversation to see the command output if you need to be sure.
- **A check is only as good as what you write.** A weak test suite passes weak work, and a check
  the lead has to judge by reading files, rather than by running a command, is less strict.
- **Commands still need your approval.** Running `npm test` is a command like any other: it waits
  in the Decision Inbox unless your auto-approve level allows it.
- **Stopping a task still ends the mission**, whatever the checks say, because you chose to cut
  it short.

**Run again** starts the new mission with the same checks.

### Limiting the tokens a mission uses

A mission has no token limit unless you give it one. To set one, tick **Limit the tokens this
mission can use** in the cog's **New mission options** and type how many, such as `200000`,
`200k` or `1.5m`. The limit shows as a chip on the new-mission screen, such as **Limit 200k
tokens ✕**; click the chip to start without one. It applies to the next mission you start, and
stays chosen for the ones after it until you clear it or close Humbug. A mission keeps the limit it
was started with, across restarts. While a limited mission runs, its card
shows how much of the limit it has used, such as **12.4k of 200k tokens**.

When the mission has used its limit, Mission Control stops its work where it is, as pausing
does, and the mission shows **Token limit reached**. Nothing more is spent until you choose one
of two ways on:

- **Add tokens and continue**: type how many more tokens it may use, and it carries on. They are
  counted from what it has already used, so a mission that went a little past its limit gets the
  full amount you added.
- **Remove the limit and continue**: the mission carries on with no limit.

**Continue** is not offered on its own, since the mission would only stop again straight away.
You can also stop the mission instead.

The limit is a safeguard, not an exact ceiling. Tokens are counted when a reply finishes, so the
reply that crosses the limit is finished and counted, and the mission can end up a little over
it; replies from helpers working at the same time can add a little more before they are stopped.
Tokens are counted only while Humbug is following the mission. A message you send to a mission
waiting at its limit still goes to the lead and is answered, since you chose to send it.

### Templates

A request you make again and again can be kept as a template. Type it in the box, click **Save
what you typed as a template** in the cog's **New mission options**, and give it a name. On the
new-mission screen your templates are listed just above the box, under **Your templates**: click one to put its request in the box, ready to change or send
as it is, and click its **✕** to delete it. Saving under a name you already use replaces that
template. Templates belong to the mindspace and are kept with its settings, so each project has
its own.

The box works like a conversation's input box:

- Click the paperclip to attach a file. Text and code files are attached as they are, and PDF and Word (`.docx`)
  files as the text in them. You are asked first if a file is larger than 100 KB. Each attached file is shown
  above the box; click its **×** to remove it. The files go to the mission lead with your message, and you still
  need to type a message to send them.
- Click the cog to change the AI settings, such as the model. On the new-mission screen the cog opens **New mission
  options** first, where **AI settings…** opens them. Before a mission starts, these are the settings its
  lead starts with. With a mission selected, they are its lead's settings, and apply to its next reply. As in a
  conversation, they last until Humbug closes; **Apply to all** also makes them the mindspace's default.

When you are guiding a mission, the box does not repeat the mission's name, since the mission is the one on
show. Before a mission starts, it is marked **New mission**.

Humbug then:

1. Records the mission.
2. Starts a new conversation, the **mission lead**, in Autonomous mode, so it has no tab. Its
   transcript is named with `Mission-` and listed under the mission in the **Missions** view of
   the left panel, not among your other conversations. While the mission's tab is the current tab
   and the Missions view is showing, the mission is highlighted there, just as a conversation tab
   highlights its own.
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

### When a mission gets stuck

Mission Control watches for a mission that is getting nowhere, so it does not run on
indefinitely, and shows it as **Looks stuck** with a note saying what it saw:

- **The lead keeps being asked to continue without finishing a step.** After three continues in
  a row that finish no step, Mission Control stops asking. Sending the lead a message, or the lead
  finishing a step, starts the count again.
- **A conversation keeps doing the same thing and getting the same answer.** When the lead or one
  of its helpers makes the same tool call four times in a row and gets exactly the same result
  each time (reading a file that is not there, or running a command that fails the same way),
  Mission Control cancels that conversation's reply and pauses the mission. A call whose result
  changes, such as checking on a build that is still running, is not counted.
- **The done checks keep failing.** After the lead has said it is done three times with a
  [done check](#done-checks) still failing, Mission Control stops sending it back. **Carry on**
  gives it three more tries.

**Carry on** lets the mission continue, and sending the lead a message also clears it. Only
missions in Autonomous mode are watched this way: in Classic mode you are watching the
conversation yourself.

These are rules of thumb, not an understanding of the work. A mission can go round in circles in
ways they do not catch, such as alternating between two actions, and a legitimate repeat (waiting
for something that has not changed yet) can be taken for one. In that case press **Carry on**.

### When the AI service cannot be reached

If the lead's reply fails because the AI service could not be reached, refused for being too busy
(a rate limit), or had a server error, the mission shows **Reconnecting** rather than **Failed**,
and tries the same request again by itself: after 15 seconds, then 30 seconds, 1, 2, 5 and 10
minutes. The mission card says when the next attempt is due (hover over it for the AI service's own
error), with **Retry now** to try straight away. **Pause** and **Stop** cancel the wait; continuing
a paused mission starts the attempts again. Once the AI service answers, a later failure gets every
attempt again.

If every attempt fails, the mission is shown as **Failed**, and the activity feed says the attempts
stopped. Errors that waiting will not fix, such as a rejected API key or a request the service
cannot accept, are shown as **Failed** straight away. A mission that had failed when Humbug closed
is not retried by itself when it opens again. Missions in Classic mode are not retried
automatically.

### Being told when a mission needs you

Mission Control tells you when:

- a mission is waiting for a decision only you can make (one an AI reviewer is still considering
  is not announced until it is left to you),
- a mission is accomplished, or the lead has answered your question,
- a mission has failed for good, looks stuck, or has reached its token limit.

While you are working in Humbug, the mission's tab, if it is open, is highlighted as having
something new, and the count on the Missions view's rocket icon shows how many requests are waiting.
While Humbug is in the background, it also asks for your attention the way your system does
(bouncing the Dock icon on macOS, flashing the taskbar button on Windows and Linux) and shows a
notification naming the mission; clicking the notification opens the mission. Whether the
notification appears depends on your system: some desktops do not support them, and your system
settings may hide them. What had already happened when the mindspace was opened is not announced.

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

A mission you had paused stays **Paused** instead: it is not resumed until you click **Continue**,
which also does what **Resume** would.

A mission followed in Mission Control has no tab of its own to warn you, so before Humbug quits, or
the mindspace is closed or switched, it names the missions whose lead is still replying and asks
whether to stop them. Choosing **No** leaves everything open. Choosing **Yes** stops their current
reply, and they are shown as **Interrupted** the next time the mindspace opens.

Click **Resume** to carry on. Humbug opens the lead conversation if it is not already open and
tells the lead that it was interrupted, so that it first checks what was actually finished (an
action or a delegated task may have stopped part way) and then continues with the next unfinished
step. If it was interrupted before it had reported a plan, it is asked instead to carry on with your
request: to plan it first if it is a big task, or to give the answer if that is all it needs. From there the mission carries on by itself as usual. You can also send the lead a message
instead, which resumes the mission too, or click **Stop** to leave it stopped.

To have interrupted missions pick up without clicking **Resume**, turn on **Auto-resume after
restart** at the top of Mission Control. This is saved for the mindspace, as a
[mindspace setting](mindspace-settings.md#mission-control) you can also change in **Settings**, and
is off until you turn it on. Interrupted missions are then resumed as soon as the mindspace opens, whether or not the
mission's tab is open.

A mission that was waiting for you when Humbug closed (asking a question, paused, or finished) is
not interrupted and is shown as it was. Anything the lead was in the middle of when Humbug closed,
such as an approval it was waiting for, is not carried over; resuming starts a new reply.

---

## The mission dashboard

When a mission is selected, Mission Control shows, from the top:

- **A reminder when something needs you**: while requests wait in the Decision Inbox, a banner
  says how many, and **Review now** takes you to the oldest one. The banner sits above the page
  rather than in it, so it stays in view however far you scroll, at any window width. The Decision
  Inbox itself gains an amber border, and Recent Activity gives up some of its height, until
  nothing is waiting.
- **Working on your message**: from the moment you send the lead a message, including a follow-up
  to a mission that is already done, until the lead replies to it, a strip at the top names your
  message and says it is being worked on. It is not shown if the lead's reply fails or the mission
  is stopped or interrupted; the mission card says so instead.
- **The mission card** with the mission's status, title and summary. Beside the status, a mission
  that is working says how long it has been at it, such as **Working for 4m**, counted from when
  it last set to work and kept up to date while you watch; a mission that is not working says when
  it was started. Mission Control does not estimate how long is left: the lead cannot know how
  long its next steps will take, so any figure would be a guess. Next to that is how many tokens the
  mission has used so far, such as **12.4k tokens** (or **12.4k of 200k tokens** for a mission with a
  [token limit](#limiting-the-tokens-a-mission-uses)), counting its lead and every helper; hover over
  it to see input and output separately. The same figure is in the final report. Tokens are counted
  only while Humbug is following the mission, so missions from before this count was kept start at
  none, and Humbug has no price list, so it shows tokens rather than money. Once there is a plan, a ring
  beside the title fills with how much of it is done (a task in progress counts as half done), with
  how many steps are done and the step that comes next, or the step that is blocked. Under the
  title, a journey bar shows where the mission is on its way, one slim bar for each stage:
  **You asked**, **AI plans it**, **AI team works** and **Job done**. Stages passed are filled, the
  current one fills with the share of the plan that is done, and while the AI is working a sheen
  travels along it; it turns amber when the mission is waiting for you.
  Below the journey, a tinted strip sums the mission up. **Now** gives what the lead is doing,
  in its own words; it is left out while the mission is interrupted, paused, reconnecting, stuck
  or failed, because a notice on the card says so instead, with what you can do about it. Before
  there is a plan, the strip also says how far the mission has got, such as the work finished
  before a follow-up started new work. A finished step is never shown as the mission being done. Requests waiting for you are not repeated here: they are in the banner and the
  Decision Inbox. **Hide details**, beside the mission's buttons, folds away the plan,
  the agents, the conversation, earlier results and recent activity, leaving the mission card, the
  lead's latest message and the Decision Inbox; **Show details** brings them back. Classic missions
  do not have the summary. Under the **Auto-approve** choice, a sentence says what the chosen level
  means.
- **Mission accomplished**: when every step is done and nothing you sent is still waiting for a
  reply, a report card appears at the top of the dashboard. A message you send to a finished
  mission makes it **Running** again until the lead has answered it. If you watched the mission
  finish, it is celebrated with a burst of confetti, once: opening it later, or the lead answering
  a follow-up, does not celebrate it again. The card is headed by the mission's name under
  **Mission accomplished**, with how many steps were done and how many agents took part. Below a
  divider is the lead's final report, which spans the card's full width; its headings are drawn no
  larger than bold text, so they never outweigh the mission's name. A finished mission's page is
  its report and the Conversation beside it: the mission card, the Agents and the empty Decision
  Inbox are put away. Under the mission's name and figures is a row of buttons. **Copy report**
  puts the report on the clipboard twice over: formatted, so that pasting into a document, a mail
  or a chat that keeps formatting gives its headings, lists, bold text and tables, and as the lead
  wrote it, in Markdown, for anywhere that takes only plain text. Which one you get is decided by
  where you paste, and the formatted copy takes that document's own fonts and colours rather than
  Humbug's theme. You can also select any part of the report with the mouse.
  **New mission** and **Ask a follow-up** (to continue the same mission) follow, and the **⋯** menu
  has **Switch to Classic mode** and **Delete**. Under the report, **In this report** offers each file in the mindspace the report names, to open
  in an editor tab; names of files that do not exist are left out. The report's **⋯** menu also has
  **Save as a Word document…**, which keeps the report as a `.docx` with its headings, lists and
  tables (a copy saved inside the mindspace is recorded in the change journal like any other
  change), and **Copy as an image**, which puts a picture of the report card, exactly as it looks
  on screen, on the clipboard. **Show the steps**, under the report, brings back
  the Mission Plan, and **Show what happened** brings back Recent Activity; each folds them away
  again. The Decision Inbox appears whenever a request is waiting. Sending a follow-up puts the
  mission back to work, and all its cards return. While you
  read a long report, its headline (**Mission accomplished** with the steps done and agents on the
  team) stays pinned at the top of the view, over the card itself, once the card's own headline has
  scrolled away, and goes once you have scrolled past the report. **Back to top** on the pinned
  headline takes you back to the start of the report. While the report is showing, the lead's last message is not repeated in
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
  the lead reports progress and asks you questions. It is a plain card until the lead has stopped
  to wait for you; then the card is highlighted and marked **Waiting for your reply**: click **Reply**, or type in the box at the
  bottom, to answer it. The **Reply** button only appears when the lead is waiting for you.
- **Agents**: a live picture of who is working on the mission, across the full width directly
  under the mission card, so you see the team at work before anything else. The lead is at the
  top, and each delegated agent below the agent that handed it the work. Each agent stands in a
  soft pool of light tinted by what it is doing: the accent colour while it works or thinks, amber
  while it waits for you. Robots come in two looks: **Classic**, square-headed with an antenna,
  and **Rounded**, with a rounder head and a flared body. **Lead's look**, at the top of the
  card, chooses which the lead of every mission has, and is remembered for you; helpers come in
  both looks, each keeping the same one. When Humbug is set to Arabic the robots wear traditional
  dress: a Classic robot a white kandura with a ghutra held by a black agal, and a Rounded robot
  a black abaya with a shayla. The light that shows what an agent is doing then sits at the
  neck of the robe instead of on an antenna. Every agent has a
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
  what's next… (2 actions so far)". A tag under each agent's name shows the display name of the AI
  model it uses, and the same model is named in **Show as a list** and when you hover over
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
  or Blocked, below the Agents. Click a step to open it and see the lead's latest
  progress note, the evidence for a completed step, and the delegated conversation working on it,
  with a button to open that conversation. Click the step again to close it. What a step shows is
  only as complete as the lead's report: a step the lead has said nothing about shows that no
  progress has been reported. The filters in the plan's heading, **All**, **Completed** and
  **In progress**, each say how many steps they show: choose one to show only those steps, and
  choose it again, or **All**, to see every step.
- **Done Checks**: shown only for a mission started with [done checks](#done-checks), straight under
  the mission card: each check with **Passed**, **Failed** or **Not run yet** and the lead's evidence,
  and how many have passed.
- **Files Changed**: every file the mission's lead and helpers created, changed or deleted, newest
  first, with how many times. A line at the top sums it up, such as **2 created · 4 changed · +221
  −20 lines**. Each file is named first, with its folder underneath, and shows how many lines the
  mission added (green) and removed (red), with a row of five blocks showing the share of each, so
  a one-line fix and a rewrite look different at a glance. The counts compare the file before the
  mission with the file as the mission left it, so your own later edits are not included; files
  that are not text, or are larger than 1 MB, have no counts. A file the mission wrote but left
  with the same lines it started with (rewritten unchanged, or changed and then changed back) is
  not listed, because there is nothing in it to review; a change only to its line endings counts
  as no change here.
  Click a text file's row (or press Space or Return on it) to unfold what changed, the way a code
  review shows it: removed lines in red and added lines in green, with each line's number before
  and after, and three unchanged lines around each change. Longer runs of unchanged lines are
  folded into a single row saying how many there are. Long lines are cut short rather than wrapped.
  Up to 400 lines are shown; past that, the row says how many more there are. Click the row again
  to fold it away. An unfolded file stays unfolded while the mission keeps working, and its lines
  are updated as the mission changes it again. Each file has up to three small icon buttons on the
  right; hover over one to see what it does. **See changes** (±) opens a diff of a changed or
  deleted file as it was before the mission against the file as it is now, so it does include
  later edits; a file the mission created has nothing earlier to compare with, so it has no such
  button. **Open** (an arrow leaving a box) shows a file in an editor tab, and **Undo** (a curved
  arrow) puts it back as
  it was before the mission changed it: a file it created is removed, and a file it changed or
  deleted gets its earlier content back. **Undo all** does that for every file the mission changed.
  Each asks you first. The list comes from Humbug's [change journal](journal.md), so the same
  limits apply: only changes made through Humbug's own tools are recorded, and a command the
  mission ran in a terminal can change files without them appearing here. A file that has been
  changed again since, by you or anything else, is marked **Changed since** and is not undone, so
  your later work is never overwritten; one you have put back is marked **Undone**. The list stays
  on a finished mission's page, under its report.
- **Recent Activity**: a scrolling feed, newest first, as tall as its entries up to a limit, of what has happened in this mission
  since Mission Control was first opened in this mindspace. It includes the objective you started
  it with, each message you sent from Mission Control, and anything you typed directly into one of
  the mission's conversations. Tools are described by what they did, such as **Nova read
  install.md** or **Nova ran make docs**, and the same thing happening several times in a row is
  one entry with how many times, such as **×4**, at the time it last happened. Entries marked **▶** have more to show: click one to open it and see
  the whole story, such as your full message, the full approval request, the file or command a tool
  worked on, or the reviewer's reasoning, and click it again to close it.
- **Conversation**: in the right-hand column, below Recent Activity. It shows everything you and the
  lead have said, oldest first, as you would read it in a conversation tab but without the input box (you reply from the box at
  the bottom of Mission Control). A long conversation opens at its latest 40 entries; click **Show earlier
  messages** above them to add 40 more at a time, or open the lead's conversation to read it all at once. Your messages sit on the right, shown exactly as you typed them;
  the lead's replies are on the left under its name, formatted and in full. Beside the name, a tag
  shows the AI model that wrote that reply. Between them, a small
  **⚙** line names each tool the lead used and what it worked on, such as
  `filesystem.write_file · notes.md`. Your first message is shown as the request you typed, not the
  instructions Mission Control adds to it, and Mission Control's own requests for the lead to carry
  on are left out. The count beside the heading is how many messages you have sent. Each message is
  always as tall as its text. The conversation takes only the room it needs, up to a set height;
  beyond that it scrolls inside the card, with the newest message in view unless you have scrolled up
  to read an earlier one.

Whenever Mission Control moves the page for you, such as **Review now**, **Read more**,
opening a plan step or the plan history, or opening an activity entry, it glides there
rather than jumping, so you can see where you have been taken.

### Finding and managing missions

Your missions are listed in the **Missions** view of the left panel, the rocket icon on the rail
just below Conversations. Click a mission there to open its tab, or **+** to start another. Each mission shows a coloured dot and its status, and a mission waiting for your decision
shows an amber count of the requests waiting. The rocket icon itself carries the total, so you can
see that something needs you whichever view the panel is showing. While the panel is collapsed, the
count still shows on the rail.

Once there are more than a few missions, a **Search missions** box at the top of the view shows only
those whose name or request contains every word you type. Right-click a mission for:

- **Open in Mission Control**, in a tab that stays.
- **Rename**, to give the mission a name of your own. The name stays, even when the lead later
  reports a plan with a different title.
- **Run again**, which starts a new mission with the same request, lead AI settings, choice to
  review the plan and [done checks](#done-checks), and with the same auto-approve level, except
  **Everything**, which has to be chosen and confirmed afresh, so the new mission starts at your
  default level instead. The new mission opens in a tab of its own. The report's **⋯** menu offers
  it too.
- **Archive**, which puts a mission away without deleting anything. Archived missions are left out
  of the list; **Archived (n)** at the bottom of the view shows them, and **Bring back from the
  archive** returns one. A mission that is working or waiting for you cannot be archived, so
  nothing that needs you goes out of sight.
- **Delete**, which removes the mission from Mission Control after you confirm. Its conversations
  are kept, but a lead followed in Mission Control is stopped first, and any standing approval the
  mission gave its conversations is withdrawn.
- **Switch to Classic mode** or **Switch to Autonomous mode**, whichever it is not in.

When a mission other than the one you are looking at is waiting for you, a banner at the top of
the tab names it (or says how many are waiting), and **Go there** opens its tab. **New mission** on
a finished mission's report opens a new-mission tab too, so the report stays where it is.

### Mission status

| Status | Meaning |
|---|---|
| **Planning** | The lead is working and has no unfinished plan yet: it is planning the mission, a follow-up you sent after it finished, or the next part of your goal |
| **Running** | The lead or a delegated agent is working through the plan, including while a request is being approved automatically and in the moment before the lead is asked to carry on |
| **Needs you** | A conversation in the mission is waiting for your approval |
| **Waiting for you** | Nothing is running; the lead may have asked you a question |
| **Answered** | The lead answered your question; no work was needed, so there is no plan |
| **Reconnecting** | The AI service could not be reached; Mission Control will send the lead's request again by itself, and the mission card says when. See [When the AI service cannot be reached](#when-the-ai-service-cannot-be-reached) |
| **Plan ready** | You asked to review the plan first, and the lead has drafted it. See [Reviewing the plan first](#reviewing-the-plan-first) |
| **Looks stuck** | Mission Control stopped driving the mission because it was getting nowhere. The mission card says why, with **Carry on**. See [When a mission gets stuck](#when-a-mission-gets-stuck) |
| **Token limit reached** | The mission used the tokens you allowed it, and its work is paused. The mission card offers **Add tokens and continue** and **Remove the limit and continue**. See [Limiting the tokens a mission uses](#limiting-the-tokens-a-mission-uses) |
| **Failed** | The lead's latest reply ended in an error it could not recover from, or every automatic retry failed. Where the kind of error is known, the mission card says what went wrong and what to do: a rejected API key, a model that was not found, a request too long for the model, a rate limit, no connection, trouble at the AI service, or no AI backend for the model's provider; hover over it to read what the AI service said. The mission card says so, with **Retry** to send the same request to the lead again (the error and any partial reply are removed first), and **Open the lead's conversation** to see why. Sending a message also tries again. **Resume** is offered only to an **Interrupted** mission |
| **Completed** | The lead reports that everything you asked for is done, every task in its plan is completed, and every [done check](#done-checks), if you set any, has passed |
| **Paused** | You paused the mission: nothing is approved automatically and the lead does not carry on by itself until you click **Continue** |
| **Stopped** | You stopped the mission |

### Mission actions

**Pause** and **Stop** are in the mission card. **Switch to Classic mode** and **Delete** are in the **⋯** menu
beside them, so a mission cannot be deleted by a slip of the mouse aimed at **Stop**.

- **Pause** stops whatever the lead and its helpers are doing straight away, as **Stop** does,
  and holds the mission so you can look at the work; a request they were waiting on is rejected.
  An action already under way when you pause, such as a command running in a terminal, may still
  finish, because it is cancelled from the conversation's side. While it is paused, nothing is approved automatically, whatever its auto-approve level:
  every request waits in the Decision Inbox. The approval a conversation may hold for file
  changes for the rest of its turn is withdrawn, and the lead is not asked to continue. A
  paused mission stays paused if Humbug restarts, and **Auto-resume after restart** leaves it
  alone. **Continue** lifts the pause: requests still waiting are considered again under the
  mission's auto-approve level, and the lead is asked to carry on if there is work left. If the
  pause stopped the lead part way, it is told so, checks what was actually finished, and carries
  on from there; the stopped reply is not shown as an error.
  Messages you send while it is paused are delivered as usual.
- **Switch to Classic mode** opens the lead conversation as a tab beside Mission Control and
  moves the mission to Classic mode.
- **Stop** cancels the lead and every delegated conversation in the mission, as **Esc** does in
  a conversation tab. Any approval they were waiting for is rejected. Stopping a paused mission
  ends the pause.
- **Delete** removes the mission from Mission Control after you confirm. Its conversations are
  kept; a lead followed in Mission Control is stopped first.

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
- Right-click the mission, or its conversation, in the **Missions** view of the left panel and
  choose
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

Opening an Autonomous mission's conversation from the Missions view also
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

At **Everything**, Mission Control never asks you to answer the mission's requests: a request may
appear in the Decision Inbox for a moment while it is approved, but its card has no **Approve**,
**Unsure** or **Reject** buttons. While such a mission is paused its requests wait, and each card
says it will be approved when you continue the mission; to answer one yourself instead, choose a
lower level first. This applies only in Mission Control. A conversation tab still shows its own
approval buttons.

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
not mixed in: they show in their own mission's tab, and the count in the Missions view and the
banner described above tell you they are waiting. Requests from conversations that are not part of
any mission show in every open Mission Control tab; answering one in any tab answers it everywhere. Each request shows the tool, the reason it gave, any
detail such as a diff, which conversation asked, and whether the operation is destructive.
Where the request names what it acts on (a file, a move from one path to another, a command or a
URL), that is shown as **Acts on**. A destructive request also says, before you approve it,
whether it can be undone: a file change that Humbug's file change journal records can be, and
anything else, such as a terminal command or a network request, cannot be undone by Humbug. With
nothing waiting, the inbox is a single line marked **All clear**.

Requests can be answered from the keyboard once you have clicked on the Mission Control page
(the keys are ignored while you are typing in the box at the bottom):

| Key | Does |
|---|---|
| **A** | Approves the selected request |
| **Shift+A** | Approves a destructive request. **A** alone only brings it into view, so you can see what it acts on and whether it can be undone first |
| **R** | Rejects the selected request |
| **U** | Answers **Unsure** for the selected request |
| **N** / **P** | Selects the next or the previous request |

The first request is selected to begin with, and once you start using the keys the selected one
is ringed. The inbox lists the keys under its requests.

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
