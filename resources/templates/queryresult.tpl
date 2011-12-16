<html>
	<title>Query Result</title>
	<body>
    <div class="message-form">
    	<form action="/getmsg">
    		<fieldset>
    			<legend>Get message with somenum</legend>
    			<label for="msg">somenum</label>
    			<input type="text" id="msg" name="num" />
                <input type="submit" value="Submit" />
    		</fieldset>
    	</form>
    </div>
    <div class="message-list">
    	<ul>
    		<messages>
    			<li><message /></li>
    		</messages>
    	</ul>
    </div>
    </body>
</html>
