require 'rails_helper'

RSpec.describe UpdateStravaDescriptionJob, type: :job do
  include ActiveJob::TestHelper
  let(:user) { create(:user) }

  before :each do
    stub_request(:post, 'https://www.strava.com/oauth/token').to_return(
      body: '{}',
      headers: {
        content_type: 'application/json',
      },
    )
  end

  describe '#perform' do
    it 'fails gracefully if lacking write permissions' do
      import = build(:import)
      stub_request(:put, STRAVA_ACTIVITIES_API_PATH).to_return(
        body:
          '{"message":"Authorization Error","errors":[{"resource":"AccessToken","field":"activity:write_permission","code":"missing"}]}',
        headers: {
          content_type: 'application/json',
        },
        status: 401,
      )
      res =
        UpdateStravaDescriptionJob.perform_now(
          user,
          import.id,
          'test description',
        )
      expect(res).not_to be_a(StravaClient::ApiError)
    end
  end
end
