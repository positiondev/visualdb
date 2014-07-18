<apply template="page">

  <div class="container">
    <apply template="_artist_about"></apply>
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
