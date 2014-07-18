<apply template="page">
  <div class="container">
    <table class="table table-striped">
    <allSubject>
      <tr>
        <td><form method="post" style="display: inline" action="${subjectsPath}/${id}/destroy"><button type="submit" onclick="return confirm('Are you sure?')">delete</button></form></td>
        <td><a href="${subjectsPath}/${id}" class="btn btn-default"><title/></a></td>
      </tr>
    </allSubject>
    </table>
  </div>
</apply>
