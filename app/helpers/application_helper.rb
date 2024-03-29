module ApplicationHelper
  def elm_flags
    flags = {}
    flags[:user] = {
      id: current_user.id,
      email: current_user.email,
      provider: current_user.provider,
    }
    flags[:rollbar_access_token] =
      Rails.configuration.rollbar_client_access_token
    flags[:environment] = Rails.env
    flags[:flash] = flash[:notice] || flash[:alert]
    tag.meta(id: 'elm-flags', data: { flags: flags.to_json })
  end
end
