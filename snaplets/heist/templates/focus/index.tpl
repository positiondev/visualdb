<apply template="page">
  <div class="container">
    <table class="table table-striped">
    <allFocus>
      <tr>
        <td><form method="post" style="display: inline" action="${focusesPath}/${id}/destroy"><button type="submit" onclick="return confirm('Are you sure?')">delete</button></form></td>
        <td><a href="${focusesPath}/${id}" class="btn btn-default"><title/></a></td>
      </tr>
    </allFocus>
    </table>
  </div>
</apply>
