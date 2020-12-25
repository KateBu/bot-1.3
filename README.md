# Update info
## Telegram bot works properly and supports any types of messages (with or without attachments). 
## VK bot can process text messages, /repeat and /help commands, map coordinates, links, stickers, audio, video attachments. If there is an attachment the VK bot cannot process, the attachment will be ignored. 


# Echo bot for Telegram and VK 

<p> The bot is not completed yet, it doesn't have any unit-tests. </p>

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




