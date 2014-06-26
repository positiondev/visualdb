<apply template="auth">
  <form class="form-signin" role="form" method="POST">
    <h2 class="form-signin-heading">Please sign in</h2>
    <err>
      <div class="alert alert-danger"><msg/></div>
    </err>
    <input name="login" type="text" class="form-control" placeholder="Username" required autofocus>
    <input name="password" type="password" class="form-control" placeholder="Password" required>
    <p>Don't have a login yet? <a class="btn" href="/new_user">Create a new user</a></p>
    <button class="btn btn-lg btn-primary btn-block" type="submit">Sign in</button>
  </form>
</apply>
