require 'rails_helper'

RSpec.describe UpdateStravaImportJob, type: :job do
  let(:user) { create(:user) }

  describe '#perform' do
    it 'creates new imports and activities' do
      import = build(:import)
      stub_request(:post, 'https://www.strava.com/oauth/token').to_return body:
                                                                           '{}',
                                                                         headers: {
                                                                           content_type:
                                                                             'application/json'
                                                                         }
      stub_request(
        :get,
        %r{strava.com\/api\/v3\/activities\/.+}
      ).to_return body: import.data.to_json,
                                                                           headers: {
                                                                             content_type:
                                                                               'application/json'
                                                                           }
      UpdateStravaImportJob.perform_now(user, import.id)
      expect(Import.find(import.id)).to be_truthy
      expect(Activity.find(import.id).completed_duration).to eq(
        import.data['moving_time']
      )
    end
  end
end
