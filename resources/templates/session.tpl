<html>
	<title>Sessions</title>
	<body>
        <div class="session-form">
        	<form action="/session" method="post">
        		<fieldset>
                    <legend>Add session value</legend>
                    <label for="key">Key</label>
                    <input type="text" id="key" name="key" />
                    <label for="value">Value</label>
                    <input type="text" id="value" name="value" />
                    <input type="submit" value="Submit" />
                </fieldset>
        	</form>
        </div>
		<div class="session-list">
			<ul>
				<sessions>
					<li><strong><key /></strong> => '<code><value /></code>'</li>
				</sessions>
			</ul>
		</div>
	</body>
</html>
