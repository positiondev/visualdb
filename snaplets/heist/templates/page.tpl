<apply template="base">

  <nav class="navbar navbar-default" role="navigation">
    <div class="container-fluid">
      <div class="navbar-header">
        <button type="button" class="navbar-toggle" data-toggle="collapse" data-target="#bs-example-navbar-collapse-1">
          <span class="sr-only">Toggle navigation</span>
          <span class="icon-bar"></span>
          <span class="icon-bar"></span>
          <span class="icon-bar"></span>
        </button>
        <a class="navbar-brand" href="/">Jacobin Visual DB</a>
      </div>

      <!-- Collect the nav links, forms, and other content for toggling -->
      <div class="collapse navbar-collapse" id="bs-example-navbar-collapse-1">
        <ul class="nav navbar-nav">
          <li><a href="${artistsNewPath}">New Artist</a></li>
          <li class="dropdown">
            <a href="#" class="dropdown-toggle" data-toggle="dropdown">All Artists <span class="caret"></span></a>
            <ul class="dropdown-menu" role="menu">
              <allArtists>
                <li><a href="${artistsPath}/${id}"><name/></a></li>
              </allArtists>
            </ul>
          </li>
          <li><a href="${subjectsIndexPath}">Subjects</a></li>
          <li><a href="${subjectsNewPath}">New Subject</a></li>

          <li><a href="${focusesIndexPath}">Focuses</a></li>
          <li><a href="${focusesNewPath}">New Focus</a></li>


        </ul>
        <form class="navbar-form navbar-left" role="search">
          <div class="form-group">
            <input type="text" class="form-control" placeholder="Search">
          </div>
          <button type="submit" class="btn btn-default">Submit</button>
        </form>
        <ul class="nav navbar-nav navbar-right">
          <li><a href="/logout">Logout</a></li>
        </ul>
      </div><!-- /.navbar-collapse -->
    </div><!-- /.container-fluid -->
  </nav>

  <requireLogin/>

  <apply-content/>

</apply>
