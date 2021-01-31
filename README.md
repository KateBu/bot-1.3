# Echo bot for Telegram and VK 

<p> Telegram bot works properly and supports any types of messages (with or without attachments).  </p>
<p> VK bot can process text messages, /repeat and /help commands, map coordinates, links, stickers, audio, video attachments. If there is an attachment the VK bot cannot process, the attachment will be ignored.   </p>

## Project structure description

<p> The project is devided into three parts: app (Main module), src (see the structure of this part below), test (contains unit tests). </p> 

### Src structure

<p> src part contains the following folders: </p>

- API  
- Config  
- Environment 
- Exceptions  
- Logic  
- Services  

#### API 
<p> API folder contains modules Bot (runBot and nextLoop functions), Messages (contains parseFailMessage) and Wrapper (exports getPureMessageList and sendM functions) </p>

<p> The <b>Telegram</b> folder contains module Cleaners (exports telByteStringToPureMessageList function and contains functions for decoding json data Telegram server sends into list of PureMessages), there are some functions for decoding in Cleaners folder. Module Data contains some specific data for connecting to Telegram server and some functions for building Telegram http requests. You can find specific data structures and FromJSON/ToJSON instances of Telegram server responses in Structs folder. </p>

<p> The <b>VK</b> folder contains module Data (specific data for connecting to VK server and some functions for building VK http requests). Module Structs exports specific data structures and instances for VK server responses (all the structures are located in Structs folder). The structure of VK message is more complicated than Telegram's one so you can find functions for decoding VK response bytestrings into PureMessages in separate folder Cleaners.  </p>


<p> to be continued ... </p>



## What the bot can do 

<p> The bot can get a message from user (any kinds of messages) and then send the message back to user several times. If user sends "/help" command, bot sends user some information about itself. If user sends "/repeat" command, bot sends user an inline keyboard, so user can choose the number of repetitions.</p>

## How to run

- clone the repo
- build project with stack
- check and fill in a config.cofig file (see below)
- create a database (see below)
- run an execute file

## How to fill in the Config File (config.config):

<p><b>botType</b>: Either "VK" or "Telegram" (quotes are required); </p>
<p><b>repetition</b>: an Int (recommended from 1 to 5);</p>
<p><b>telegramToken</b>: put your Telegram Token here if you want to run a Telegram Bot (quotes are required);</p>
<p><b>VKToken</b>: put your VK Token here if you want to run a VK Bot  (quotes are required);</p>
<p><b>VKGroupID</b>: an Int, put your VK Group ID here if you want to run a VK Bot;</p>
<p><b>helpMessage</b>: you can change the message a user will get if sends /help command (quotes are required);</p>
<p><b>logPriority</b>: use "Debug", "Info", "Warning" or "Error" (quotes are required).</p>

## How to create a database:
<p>The bot works with a primitive database (PostreSQL), that contains one table to store user IDs and number of repetition for every user. The database supposed to be hosted locally (the same computer as the bot). So to create the database you just have to write a few strings in psql command line: </p> 

- to create a new user 'bot' with password 'bot13': <br> 
    <b> CREATE USER bot WITH password 'bot13'; </b>
- to create a new database 'users': <br> 
    <b> CREATE DATABASE users; </b>
- to grant the database to user 'bot':<br> 
    <b> GRANT ALL ON DATABASE users TO bot; </b>
- then run the database 'users' and create a new table: <br>
    <b>  CREATE TABLE UserRepeats <br>
    ( userId char (15) NOT NULL, <br>
    repeats integer NOT NULL, <br>
    CHECK ( repeats > 0 and repeats < 6), <br>
    PRIMARY KEY ( userId ));</b>
- that's all

## How it works
<p> You can see some screenshots below. </p>

### Telegram bot 
<p> Usual text message (default number of repetition is 2) and /help command response </p>
<p> <img src="https://user-images.githubusercontent.com/30144022/103772217-30ae8100-503a-11eb-9e66-017d6053ddae.png">
</p>

<p> /repeat command response and text message (number of repetition is 3) </p>
<p> <img src="https://user-images.githubusercontent.com/30144022/103865354-1fff1900-50d5-11eb-8fc8-e904bf8b7c40.png">
</p>

### VK bot 
<p> Usual text message (default number of repetition is 2) and /help command response </p>
<p> <img src="https://user-images.githubusercontent.com/30144022/103773538-46bd4100-503c-11eb-806f-f7ff7d75f1ed.png">
</p>

<p> /repeat command response and text message (number of repetition is 5) </p>
<p> <img src="https://user-images.githubusercontent.com/30144022/103865404-360cd980-50d5-11eb-98b7-14634ea570d5.png">
</p>

