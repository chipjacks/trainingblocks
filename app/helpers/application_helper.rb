module ApplicationHelper
  def elm_flags(flags)
    flags = flags || {}
    flags[:rollbar_access_token] = Rollbar.configuration.access_token || ""
    flags[:environment] = Rails.env
    tag.meta(id: 'elm-flags', data: {flags: flags.to_json})
  end
end
