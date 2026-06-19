# Force the bundled manifest skeleton for the entire test suite.
#
# Without this, every TemplateFlowClient()/tf_cli() call triggers
# tf_latest_skeleton(), which fetches the remote skeleton MD5 from GitHub and
# (because the bundled snapshot rarely matches the live remote) downloads and
# unzips the full skeleton archive -- ~30-45s and high CPU per client. Across
# the suite that added up to >20 minutes and made R CMD check network-dependent.
#
# The bundled skeleton is complete enough for all listing/metadata/entity
# assertions (the parity suite already relies on it). Tests that exercise real
# file downloads still go to S3 and guard themselves with skip_if_offline();
# this option only changes where the manifest index comes from, not downloads.
old_opts <- options(templateflow.test.forcebundled = TRUE)
withr::defer(options(old_opts), teardown_env())
