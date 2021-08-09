require 'rails_helper'

RSpec.describe InitialStravaImportJob, type: :job do
  let(:user) { create(:user) }

  describe '#perform' do
    it 'creates new imports' do
      imports = build_list(:import, 3).map { |i| i.data }
      imports.each_with_index { |import, index| import['id'] = index }
      stub_request(
        :get,
        %r{strava.com\/api\/v3\/athlete\/activities.+}
      ).to_return body: imports.to_json,
                                                                                  headers: {
                                                                                    content_type:
                                                                                      'application/json'
                                                                                  }
      expect { InitialStravaImportJob.perform_now(user, '') }.to change {
        Import.count
      }.by(3)
    end
  end
end
