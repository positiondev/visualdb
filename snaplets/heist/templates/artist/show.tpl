<apply template="page">

  <div class="container">
    <div class="panel panel-default">
      <div class="panel-heading">
        <strong><name/></strong>
        <a class="btn btn-default" style="float: right" href="${artistsPath}/${id}/edit">Edit</a>
        <div style="clear: right"></div>
      </div>
      <div class="panel-body">
        <strong>Email:</strong> <email/><br/>
        <strong>Website:</strong> <a href="${website}"><website/></a><br/>
        <strong>Agency:</strong> <agency/><br/>
        <strong>Location:</strong> <city/>, <country/><br/>
        <strong>Focus:</strong>
        <a href="${artistFocusesNewPath}?artist_id=${id}" class="btn btn-info">add</a>

        <focuses><span class="btn btn-default"><title/></span>
          <form method="post" style="display: inline" action="${artistFocusesPath}/${id}/destroy"><button class="btn btn-danger" type="submit" onclick="return confirm('Are you sure?')">x</button></form>
        </focuses>
        <br/>

        <strong>Subjects:</strong>
        <a href="${artistSubjectsNewPath}?artist_id=${id}" class="btn btn-info">add</a>

        <subjects><span class="btn btn-default"><title/></span>
          <form method="post" style="display: inline" action="${artistSubjectsPath}/${id}/destroy"><button class="btn btn-danger" type="submit" onclick="return confirm('Are you sure?')">x</button></form>
        </subjects>
        <br/>
        <strong>Notes:</strong><p><notes/></p>
      </div>
    </div>
    <div class="panel panel-default">
      <div class="panel-heading">
          <strong>Images</strong>
        <a class="btn btn-success" style="float: right" href="${mediaNewPath}?artist_id=${id}&referer=${artistsPath}/${id}">Add Image</a>
        <div style="clear: right"></div>
      </div>
      <div class="panel-body">
        <media>
          <div class="image" style="position: relative">
            <img style="max-width: 100%" src="${url}"/><form method="post" style="position: absolute; top: 30%; left: -50px;" action="${mediaPath}/${id}/destroy"><button class="btn" type="submit" onclick="return confirm('Are you sure?')">x</button></form>
          </div>
          <hr/>
        </media>
      </div>
    </div>
  </div>

</apply>
