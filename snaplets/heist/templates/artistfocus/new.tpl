<apply template="page">

  <div class="container">

    <dfForm role="form" action="${artistFocusesCreatePath}?artist_id=${artist_id}">
      <div class="form-group">
        <dfLabel ref="focusId">Focus</dfLabel>
        <dfInputSelect class="form-control" ref="focusId"/>
        <dfInputHidden ref="artistId" value="${artist_id}"/>
      </div>
      <dfInputSubmit class="btn"/>
    </dfForm>

  </div>

</apply>
