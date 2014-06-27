<apply template="page">

  <div class="container">

    <dfForm action="${mediaCreatePath}">
      <dfLabel ref="file">File</dfLabel>
      <dfInputFile ref="file"/>
      <dfInputHidden ref="artist_id" value="1"/>
      <br/>
      <dfInputSubmit/>
    </dfForm>

  </div>

</apply>
