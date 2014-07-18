<div class="panel panel-default">
  <div class="panel-heading">
    <strong><a class="btn btn-default" href="${artistsPath}/${id}"><name/></a></strong>
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

    <focuses><a href="${focusesPath}/${id}" class="btn btn-default"><title/></a>
      <form method="post" style="display: inline" action="${artistFocusesPath}/${id}/destroy"><button class="btn btn-danger" type="submit" onclick="return confirm('Are you sure?')">x</button></form>
    </focuses>
    <br/>

    <strong>Subjects:</strong>
    <a href="${artistSubjectsNewPath}?artist_id=${id}" class="btn btn-info">add</a>

    <subjects><a href="${subjectsPath}/${id}" class="btn btn-default"><title/></a>
      <form method="post" style="display: inline" action="${artistSubjectsPath}/${id}/destroy"><button class="btn btn-danger" type="submit" onclick="return confirm('Are you sure?')">x</button></form>
    </subjects>
    <br/>
    <strong>Notes:</strong><p><notes/></p>
  </div>
</div>
