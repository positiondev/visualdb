<apply template="page">
  <div class="container">
    <a href="${mediaNewPath}">New</a>
    <allMedia>
      <img src="${url}"/><form method="post" style="display: inline" action="${mediaPath}/${id}/destroy"><button type="submit" onclick="return confirm('Are you sure?')">x</button></form>
    </allMedia>
  </div>
</apply>
