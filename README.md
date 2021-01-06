
# Echo bot for Telegram and VK 

<p> Telegram bot works properly and supports any types of messages (with or without attachments).  </p>
<p> VK bot can process text messages, /repeat and /help commands, map coordinates, links, stickers, audio, video attachments. If there is an attachment the VK bot cannot process, the attachment will be ignored.   </p>

## What the bot can do 

<p> The bot can get a message from user (any kinds of messages) and then send the message back to user several times. If user sends "/help" command, bot sends user some information about itself. If user sends "/repeat" command, bot sends user an inline keyboard, so user can choose the number of repetitions.</p>

## How to run

- clone the repo
- build project with stack
- check and fill in a config.cofig file (see below)
- run an execute file

## How to fill in the Config File (config.config):

<p><b>botType</b>: Either "VK" or "Telegram" (quotes are required); </p>
<p><b>repetition</b>: an Int (recommended from 1 to 5);</p>
<p><b>telegramToken</b>: put your Telegram Token here if you want to run a Telegram Bot (quotes are required);</p>
<p><b>VKToken</b>: put your VK Token here if you want to run a VK Bot  (quotes are required);</p>
<p><b>VKGroupID</b>: an Int, put your VK Group ID here if you want to run a VK Bot;</p>
<p><b>helpMessage</b>: you can change the message a user will get if sends /help command (quotes are required);</p>
<p><b>logPriority</b>: use "Debug", "Info", "Warning" or "Error" (quotes are required).</p>


## How it works
<p> You can see some screenshots below. </p>

### Telegram bot 
<p> Usual text message (default number of repetition is 2) and /help command response </p>
<p> <img src="https://user-images.githubusercontent.com/30144022/103772217-30ae8100-503a-11eb-9e66-017d6053ddae.png">
</p>

<p> /repeat command response and text message (number of repetition is 3) </p>
<p> <img src="https://user-images.githubusercontent.com/30144022/103772254-4623ab00-503a-11eb-9f2f-17c23e2790e1.png">
</p>

