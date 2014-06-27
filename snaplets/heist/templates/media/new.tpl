<apply template="page">

  <div class="container">

    <dfForm role="form" action="${mediaCreatePath}?referer=${referer}">
      <div class="form-group">
        <dfLabel ref="file">File</dfLabel>
        <dfInputFile ref="file"/>
      </div>
      <dfInputHidden ref="artist_id" value="${artist_id}"/>
      <dfInputSubmit class="btn"/>
    </dfForm>

  </div>

</apply>
